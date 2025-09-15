# University timetable vision
## 1. Introduction
Manual creation of a university timetable is complex, time-consuming, and error-prone: it requires considering numerous constraints (rooms, instructors, student groups, rules), and it is difficult to quickly adjust the schedule when changes occur. The goal of this project is to create a university timetabling application that generates schedules for courses, students, rooms, and staff, subject to various academic and logistical constraints. The system should automate timetable creation while providing mechanisms to handle unsatisfiable constraints and support flexible adjustments.
## 2. Stakeholders
* Students – need conflict-free timetables that respect study load and breaks.
* Tutors/Staff – require schedules that respect availability and teaching constraints.
* Administration (timetablers) – need tools for automated timetable creation, conflict resolution, and efficient resource allocation.
## 3. Product Overview
### The University Timetable application will:
1) Generate timetables automatically based on constraints.
2) Identify unsatisfiable constraint subsets when conflicts occur.
3) Handle multiple valid solutions and ensure locality (only relevant parts of the timetable are recalculated after changes).
4) Provide configurable policies (e.g., lunch breaks, maximum daily study hours).
### Key Benefits
- Reduced manual work in timetable creation.
- Improved satisfaction of students and staff.
- Better use of rooms and facilities.
## 4. Project implementation plan: 
1) Technology Exploration
2) Architecture design 
3)	Testing and debugging 
4)	Performance optimization
5)	Documentation
## 5. Success Criteria
* The system generates valid timetables that satisfy constraints.
* Conflicts are detected and reported with clarity.
* The timetable adapts to changes without unnecessary recomputation.
