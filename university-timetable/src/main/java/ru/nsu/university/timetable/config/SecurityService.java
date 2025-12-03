package ru.nsu.university.timetable.config;

import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import ru.nsu.university.timetable.user.auth.UserRepository;

import java.util.UUID;

@Service("securityService")
@RequiredArgsConstructor
public class SecurityService {
    private final UserRepository userRepository;

    public boolean canUpdateTeacherWorkingHours(UUID teacherId) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();

        if (auth == null || !auth.isAuthenticated()) {
            return false;
        }

        boolean isAdmin = auth.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .anyMatch("ROLE_ADMIN"::equals);

        if (isAdmin) {
            return true;
        }

        boolean isTeacher = auth.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .anyMatch("ROLE_TEACHER"::equals);

        if (!isTeacher) {
            return false;
        }

        String login = auth.getName();

        return userRepository.findByLogin(login)
                .filter(u -> u.getTeacher() != null
                        && u.getTeacher().getId().equals(teacherId))
                .isPresent();
    }
}
