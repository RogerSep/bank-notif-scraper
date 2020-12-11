# Statectomy

This is the repository of the code for a talk in s4n I gave on December 9 2020.

You can find the talk in [YouTube](https://www.youtube.com/watch?v=2YapxGl8GDA&t=1705s) (it's in Spanish), and the deck [here](./Statectomy.pdf)

---
The application scrapes bank notifications from gmail and outputs the transactional
information on the "notificaciones" topic in kafka.

There's a docker-compose to ease things.

## Setup
- Clone the repository
- Follow the [quickstart guide](https://developers.google.com/gmail/api/quickstart/java) of 
the gmail api to download the `credentials.json` file and place it in the src/main/resources folder 
- The first time you run the application, you'll have to authorize the app.

## Run
```bash
# to run kafka locally using docker compose.
docker-compose up broker

# to run the application
sbt run

# to see the information in the kafka topic.
docker-compose up kafka-messages-logger

# export the results in a csv file to open as a spreadsheet. (Depending on your setup)
# choose one of these.
kafka-console-consumer --bootstrap-server localhost:9092 --topic notificaciones --from-beginning
docker logs kafka-messages-logger | tee output.csv
```