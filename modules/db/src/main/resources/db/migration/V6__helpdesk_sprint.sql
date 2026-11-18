-- ヘルプデスクのプロジェクトは、常に1つのアクティブスプリントにすべてのタスクを置く（バックログを持たない）。
-- タスクの作成・更新・取り込み・自動化・MCP のどれから入っても同じになるよう、DB のトリガーで強制する。

-- ボードで、完了してからこの日数を過ぎたタスクを出さない（ヘルプデスクのプロジェクト）
alter table project add column done_hide_days int not null default 7 check (done_hide_days between 0 and 3650);

-- プロジェクトのアクティブスプリント（なければ計画中のものを始めるか、新しく作って始める）
create function helpdesk_sprint(pid uuid) returns uuid language plpgsql as $$
declare sid uuid;
begin
  -- 同じプロジェクトで同時に作らないように
  perform pg_advisory_xact_lock(hashtext('skea.helpdesk_sprint:' || pid::text));
  select id into sid from sprint where project_id = pid and state = 'active';
  if sid is null then
    select id into sid from sprint where project_id = pid and state = 'planned' order by position, created_at limit 1;
    if sid is null then
      sid := gen_random_uuid();
      insert into sprint (id, project_id, name, goal, state, start_date, position)
        values (sid, pid, 'ヘルプデスク', '問い合わせ（自動で管理するスプリント）', 'active', current_date, 0);
    else
      update sprint set state = 'active', start_date = coalesce(start_date, current_date) where id = sid;
    end if;
  end if;
  return sid;
end $$;

-- タスクのスプリントを、ヘルプデスクのプロジェクトではアクティブスプリントにする
create function task_helpdesk_sprint() returns trigger language plpgsql as $$
begin
  if exists(select 1 from project where id = new.project_id and helpdesk) then
    new.sprint_id := helpdesk_sprint(new.project_id);
  end if;
  return new;
end $$;

create trigger task_helpdesk_sprint before insert or update of sprint_id, project_id on task
  for each row execute function task_helpdesk_sprint();

-- ヘルプデスクのプロジェクトでは、アクティブスプリントを完了・計画中に戻せない（API でも止める）
create function sprint_helpdesk_guard() returns trigger language plpgsql as $$
begin
  if old.state = 'active' and new.state <> 'active' and exists(select 1 from project where id = old.project_id and helpdesk) then
    raise exception 'ヘルプデスクのプロジェクトでは、アクティブスプリントは自動で管理します';
  end if;
  return new;
end $$;

create trigger sprint_helpdesk_guard before update of state on sprint
  for each row execute function sprint_helpdesk_guard();

-- すでにヘルプデスクのプロジェクトのタスクを、アクティブスプリントに移す
update task t set sprint_id = helpdesk_sprint(t.project_id)
  from project p where p.id = t.project_id and p.helpdesk and t.deleted_at is null;
