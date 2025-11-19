#!/bin/sh
set -e

echo "Waiting for DB..."
until pg_isready -h db -U "$POSTGRES_USER" -d "$POSTGRES_DB" >/dev/null 2>&1; do
  sleep 2
done

echo "Running schema.sql (CREATE EXTENSION pgcrypto)..."
PGPASSWORD="$POSTGRES_PASSWORD" psql -h db -U "$POSTGRES_USER" -d "$POSTGRES_DB" -f ./schema.sql

echo "Waiting for users table to be created by JPA..."

until PGPASSWORD="$POSTGRES_PASSWORD" psql -h db -U "$POSTGRES_USER" -d "$POSTGRES_DB" \
  -tAc "SELECT 1 FROM information_schema.tables WHERE table_name = 'users'" | grep -q 1; do
  echo "users table not ready, retrying in 2s..."
  sleep 2
done

echo "Running data.sql (insert root user if not exists)..."
PGPASSWORD="$POSTGRES_PASSWORD" psql -h db -U "$POSTGRES_USER" -d "$POSTGRES_DB" -f ./data.sql

echo "Root user ensured."
