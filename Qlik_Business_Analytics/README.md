# Case_Study_2

This is a basic case study I covered as a part of the Qlik Business Analyst Virtual Internship. 
On studying the dataset of an Airline company regarding their flight status and frequency of delays depending on the various factors, I came to certain conclusions. I covered the entire analysis with the help of Python programming language and the summary of analysis is attached here along with the project.

The dataset contains the following columns:
1. Passenger ID: Unique identifier for each passenger
2. First Name: First name of the passenger
3. Last Name: Last name of the passenger
4. Gender: Gender of the passenger
5. Age: Age of the passenger
6. Nationality: Nationality of the passenger
7. Airport Name: Name of the departure airport
8. Airport Country Code: Country code of the departure airport
9. Country Name: Country name of the departure airport
10. Airport Continent: Continent of the departure airport
11. Continents: Continent of the departure airport (redundant with the previous column)
12. Departure Date: Date of departure
13. Arrival Airport: Code of the arrival airport
14. Pilot Name: Name of the pilot
15. Flight Status: Status of the flight (On Time/Delayed)

Given this, we can perform a variety of analyses. Here are some key areas we'll focus on:
1. Demographic Analysis: Gender, age distribution, and nationality of passengers.
2. Flight Analysis: On-time performance, flight delays by airport, and pilot performance.
3. Geographic Analysis: Distribution of flights by continent and country.
4. Time-based Analysis: Patterns in flight departure dates.



Demographic Analysis:

![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/a16895b6-6508-4b5b-b2d6-7c5b8406b0b3)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/7d6d849c-201c-4c8a-a581-65140bb3fc6c)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/993c8f1c-ef2b-4235-aa3e-1cb4ae57473b)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/2c2ba44b-3e2c-4046-9649-d02c7407fa2f)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/061d57a5-5fec-42f5-8ed5-4f4914cc2414)

Here are the insights from the demographic analysis:
1. Gender Distribution: There is a roughly equal distribution of male and female passengers.
2. Age Distribution: The age distribution of passengers is fairly spread out, with a slight concentration in the younger age brackets. The distribution appears to be roughly normal with a peak around 30-40 years.
3. Top 10 Nationalities: The top nationalities of passengers include China, United States, and several European countries such as Russia, France, and Germany.



Flight Analysis:

![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/71a3c7d6-27ce-42bb-9d09-882da942ddae)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/783716b9-2ec9-464f-bb84-6fddcb6f96da)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/70160d37-7b9b-41b1-a9ad-f49d0de1ed38)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/48b957a8-b74d-4fef-bcc7-9f085355ff09)

Here are the insights from the flight analysis:
1. Flight Status Distribution: The majority of flights are on time, with a smaller proportion being delayed.
2. Top 10 Airports with Most Delays: Certain airports have a higher number of delayed flights. The specific airports with the most delays can be seen in the visualization.
3. Top 10 Pilots with Most Flights: The visualization shows the top 10 pilots who have flown the most flights.
4. On-time Performance of Top 10 Pilots: The stacked bar chart shows the on-time and delayed flight counts for the top 10 pilots, highlighting their performance.



Geographic Analysis:

![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/37146f62-ea42-4605-b29b-00c44b8af602)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/64bb337a-ce25-4ce6-84c9-a3a677bc6858)

Here are the insights from Geographic Analysis:

1. Distribution of Flights by Continent
    a. North America has the highest number of flights, followed by Europe and Asia.
    b. South America, Africa, and Oceania have significantly fewer flights in comparison.

2. Distribution of Flights by Country (Top 10)
    a. The United States and China are the top two countries with the most flights.
    b. Other countries in the top 10 include Canada, Russia, Japan, France, Germany, United Kingdom, India, and Brazil.
    c. These countries represent major hubs for international and domestic flights.



Time-based Analysis:

![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/c697c91b-c0dc-4564-98eb-6e557be4b9fc)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/e9882d24-b58b-4c18-9f02-99097f1a2632)
![image](https://github.com/naman-toshniwal/Data_Analytics/assets/109726889/0962bd64-e9ef-41fe-9b6c-1ec552db589a)

Here are the insights from Time-based Analysis:

1. Number of Flights Over Time
    a. The number of flights shows variability over time, with peaks and troughs indicating periods of high and low travel activity.
    b. There may be seasonal trends, such as higher travel during summer and holiday periods.

2. Top 5 Months where Passengers Traveled the Most
    a. The top 5 months with the highest number of passengers indicate peak travel periods.
    b. These months likely coincide with major holidays, summer vacation periods, and other significant travel times.

3. Top 3 Months - Flight Status Wise (Delayed/Cancelled/On-Time)
    a. During the top 3 months, a majority of flights were on time.
    b. However, there is a notable number of delayed and cancelled flights, which could be due to weather conditions, high travel demand, or operational issues.
    c. The specific months with the most flights and their respective flight status distributions provide insights into periods of operational challenges or high travel demand.
