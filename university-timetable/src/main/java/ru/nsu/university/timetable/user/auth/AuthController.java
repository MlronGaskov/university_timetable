package ru.nsu.university.timetable.user.auth;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ru.nsu.university.timetable.user.auth.dto.LoginRequest;
import ru.nsu.university.timetable.user.auth.dto.TokenResponse;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {
    private final AuthenticationManager authManager;
    private final JwtService jwtService;
    private final UserRepository userRepository;

    @PostMapping("/login")
    public ResponseEntity<TokenResponse> login(@RequestBody @Validated LoginRequest req) {
        try {
            Authentication auth = authManager.authenticate(
                    new UsernamePasswordAuthenticationToken(req.login(), req.password())
            );

            User user = userRepository.findByLogin(req.login())
                    .orElseThrow(() -> new IllegalStateException("User not found after authentication"));

            Map<String, Object> claims = new HashMap<>();
            claims.put("role", user.getRole().name());
            claims.put("userId", user.getId().toString());

            if (user.getTeacher() != null) {
                claims.put("teacherId", user.getTeacher().getTeacherId());
            }
            if (user.getStudent() != null) {
                claims.put("studentId", user.getStudent().getStudentId());
            }

            claims.put(
                    "authorities",
                    auth.getAuthorities().stream()
                            .map(GrantedAuthority::getAuthority)
                            .toList()
            );

            String token = jwtService.createToken(user.getLogin(), claims);
            return ResponseEntity.ok(new TokenResponse(token));
        } catch (BadCredentialsException ex) {
            return ResponseEntity.status(401).build();
        }
    }
}
