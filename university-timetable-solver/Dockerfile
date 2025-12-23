FROM swipl:latest

WORKDIR /app

COPY . .

EXPOSE 5000

CMD ["swipl", "--stack-limit=13g", "-O", "-q", "-s", "main.pl"]
