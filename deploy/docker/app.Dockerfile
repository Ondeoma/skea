# skea アプリ（scripts/dist.sh の成果物を詰めるだけ）
FROM eclipse-temurin:21-jre-alpine
RUN addgroup -S skea && adduser -S -G skea skea
COPY dist/lib /app/lib
COPY dist/static /app/static
ENV STATIC_DIR=/app/static HTTP_PORT=8080
USER skea
EXPOSE 8080
CMD ["java", "-XX:MaxRAMPercentage=70", "-cp", "/app/lib/*", "skea.app.Main"]
