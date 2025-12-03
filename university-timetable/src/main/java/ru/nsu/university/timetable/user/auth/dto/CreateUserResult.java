package ru.nsu.university.timetable.user.auth.dto;

public record CreateUserResult(
        UserResponse user,
        String tempPassword
) {
}
