# University Timetable

**Reference document:**  
https://docs.google.com/document/d/1GqXmejD2ltGIC_awX03aQ2I1W9C9X9F-oC3SbxMGgJI/edit?usp=sharing

> Automated university timetabling with constraint handling, alternative solutions, and local recomputation.

---

## Table of Contents
- [Introduction](#introduction)
- [Stakeholders](#stakeholders)
- [Product Overview](#product-overview)
  - [Key Features](#key-features)
  - [Key Benefits](#key-benefits)
- [Project Implementation Plan](#project-implementation-plan)
- [Success Criteria](#success-criteria)
- [Contributing](#contributing)
- [License](#license)

---

## Introduction
Manual creation of a university timetable is complex, time-consuming, and error-prone: it requires considering numerous constraints (rooms, instructors, student groups, rules), and it is difficult to quickly adjust the schedule when changes occur.  
This project aims to build a timetabling application that generates schedules for courses, students, rooms, and staff under academic and logistical constraints. It automates timetable creation, explains unsatisfiable constraints, and supports flexible, localized adjustments.

---

## Stakeholders
- **Students** — need conflict-free timetables that respect study load and breaks.  
- **Tutors/Staff** — require schedules that respect availability and teaching constraints.  
- **Administration (timetablers)** — need tools for automated timetable creation, conflict resolution, and efficient resource allocation.

---

## Product Overview

### Key Features
1. **Automatic generation** based on constraints.  
2. **Conflict diagnostics** with **unsatisfiable constraint subsets** (MUS) when issues occur.  
3. **Multiple valid solutions** with **locality**: only the relevant part of the timetable is recomputed after changes.  
4. **Configurable policies** (e.g., lunch breaks, max daily study hours).

### Key Benefits
- Reduced manual effort in timetable creation.  
- Improved satisfaction for students and staff.  
- Better utilization of rooms and facilities.

---

## Project Implementation Plan
1. **Technology exploration**  
2. **Architecture design**  
3. **Testing and debugging**  
4. **Performance optimization**  
5. **Documentation**

---

## Success Criteria
- The system generates valid timetables that satisfy constraints.  
- Conflicts are clearly detected and reported.  
- The timetable adapts to changes without unnecessary recomputation.

---

## Contributing
Contributions are welcome! Please open an issue to discuss significant changes before submitting a PR.  
Suggested steps:
1. Fork the repo  
2. Create a feature branch: `git checkout -b feature/your-feature`  
3. Commit changes with clear messages  
4. Open a pull request
