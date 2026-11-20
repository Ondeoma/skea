package skea.web

import org.scalajs.dom

import scala.scalajs.js

/** CSV の読み書き（RFC 4180. "..." の中の改行・"" に対応. 先頭の BOM は捨てる）. */
object Csv {

  def parse(text: String): List[List[String]] = {
    val s = text.stripPrefix("﻿")
    val rows = List.newBuilder[List[String]]
    var row = List.newBuilder[String]
    val cell = new StringBuilder
    var inQ = false
    var i = 0
    def endCell(): Unit = { row += cell.toString; cell.clear() }
    def endRow(): Unit = {
      endCell(); val r = row.result(); if (r.exists(_.nonEmpty)) rows += r; row = List.newBuilder[String]
    }
    while (i < s.length) {
      val c = s.charAt(i)
      if (inQ) {
        if (c == '"') {
          if (i + 1 < s.length && s.charAt(i + 1) == '"') { cell.append('"'); i += 1 }
          else inQ = false
        } else cell.append(c)
      } else c match {
        case '"' => inQ = true
        case ',' => endCell()
        case '\r' => ()
        case '\n' => endRow()
        case _ => cell.append(c)
      }
      i += 1
    }
    if (cell.nonEmpty || inQ) endRow()
    else { val r = row.result(); if (r.nonEmpty) { row = List.newBuilder[String]; rows += (r :+ "") } }
    rows.result()
  }

  def quote(v: String): String = "\"" + v.replace("\"", "\"\"") + "\""

  /** 書き出す値（= + - @ などで始まる値は、表計算ソフトで数式として動かないよう先頭に ' を付ける）. */
  def cell(v: String): String = quote(if (v.nonEmpty && "=+-@\t\r".contains(v.charAt(0))) "'" + v else v)

  /** 行（書き出した文字列）を CSV のファイルとして保存させる（表計算ソフトが UTF-8 と分かるよう BOM を付ける）. */
  def download(fileName: String, lines: List[String]): Unit =
    Dom.download(
      new dom.Blob(
        js.Array[dom.BlobPart]("\uFEFF" + lines.mkString("\r\n")),
        new dom.BlobPropertyBag { `type` = "text/csv" }
      ),
      fileName
    )

}
