package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.LoginRequest;
import ru.nsu.university.timetable.dto.TokenResponse;
import ru.nsu.university.timetable.service.JwtService;

import java.util.Map;

@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
public class AuthController {
    private final AuthenticationManager authManager;
    private final JwtService jwtService;

    @PostMapping("/login")
    public ResponseEntity<TokenResponse> login(@RequestBody @Validated LoginRequest req) {
        try {
            System.out.println(req);
            Authentication auth = authManager.authenticate(new UsernamePasswordAuthenticationToken(req.login(), req.password()));
            System.out.println(auth);
            String token = jwtService.createToken(req.login(), Map.of("authorities", auth.getAuthorities()));
            System.out.println(token);
            return ResponseEntity.ok(new TokenResponse(token));
        } catch (BadCredentialsException ex) {
            System.out.println(ex.getMessage());
            return ResponseEntity.status(401).build();
        }
    }
}