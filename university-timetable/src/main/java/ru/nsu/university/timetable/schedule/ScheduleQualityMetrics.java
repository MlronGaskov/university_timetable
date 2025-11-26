package ru.nsu.university.timetable.schedule;

import jakarta.persistence.Embeddable;

@Embeddable
public class ScheduleQualityMetrics {

    private Double totalScore;
    private Double gapsScore;
    private Double loadBalanceScore;
    private Double preferencesScore;

    public Double getTotalScore() {
        return totalScore;
    }

    public void setTotalScore(Double totalScore) {
        this.totalScore = totalScore;
    }

    public Double getGapsScore() {
        return gapsScore;
    }

    public void setGapsScore(Double gapsScore) {
        this.gapsScore = gapsScore;
    }

    public Double getLoadBalanceScore() {
        return loadBalanceScore;
    }

    public void setLoadBalanceScore(Double loadBalanceScore) {
        this.loadBalanceScore = loadBalanceScore;
    }

    public Double getPreferencesScore() {
        return preferencesScore;
    }

    public void setPreferencesScore(Double preferencesScore) {
        this.preferencesScore = preferencesScore;
    }
}
