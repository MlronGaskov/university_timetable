package ru.nsu.university.timetable;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@EnableJpaAuditing
public class UniversityTimetableApplication {
    public static void main(String[] args) {
        SpringApplication.run(UniversityTimetableApplication.class, args);
    }
}
