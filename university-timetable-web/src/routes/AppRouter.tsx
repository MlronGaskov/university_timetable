import React from 'react';
import {Navigate, Route, Routes} from 'react-router-dom';
import {PublicLayout} from '@/components/layout/PublicLayout';
import {AppLayout} from '@/components/layout/AppLayout';
import {ProtectedRoute} from './ProtectedRoute';
import {RoleRoute} from './RoleRoute';
import {LoginPage} from '@/features/auth/pages/LoginPage';
import {ProfilePage} from '@/features/profile/pages/ProfilePage';
import {CoursesListPage} from '@/features/courses/pages/CoursesListPage';
import {GroupsListPage} from '@/features/groups/pages/GroupsListPage';
import {RoomsListPage} from '@/features/rooms/pages/RoomsListPage';
import {SemestersListPage} from '@/features/semesters/pages/SemestersListPage';
import {TeachersListPage} from '@/features/teachers/pages/TeachersListPage';
import {StudentsListPage} from '@/features/students/pages/StudentsListPage';
import {UsersListPage} from '@/features/users/pages/UsersListPage';
import {PoliciesListPage} from '@/features/policies/pages/PoliciesListPage';
import {SemesterSchedulesPage} from '@/features/schedules/pages/SemesterSchedulesPage';
import {ScheduleDetailsPage} from '@/features/schedules/pages/ScheduleDetailsPage';
import {useRoleGuard} from '@/hooks/useRoleGuard';

const HomeRedirect: React.FC = () => {
    const {isAdmin, isTeacher, isStudent} = useRoleGuard();
    if (isAdmin) return <Navigate to="/admin" replace/>;
    if (isTeacher) return <Navigate to="/teacher" replace/>;
    if (isStudent) return <Navigate to="/student" replace/>;
    return <Navigate to="/profile" replace/>;
};

export const AppRouter: React.FC = () => {
    return (
        <Routes>
            <Route element={<PublicLayout/>}>
                <Route path="/login" element={<LoginPage/>}/>
            </Route>

            <Route element={<ProtectedRoute/>}>
                <Route element={<AppLayout/>}>
                    <Route path="/" element={<HomeRedirect/>}/>
                    <Route path="/profile" element={<ProfilePage/>}/>

                    <Route path="/courses" element={<CoursesListPage/>}/>
                    <Route path="/groups" element={<GroupsListPage/>}/>
                    <Route path="/rooms" element={<RoomsListPage/>}/>
                    <Route path="/semesters" element={<SemestersListPage/>}/>
                    <Route path="/teachers" element={<TeachersListPage/>}/>
                    <Route path="/students" element={<StudentsListPage/>}/>
                    <Route path="/policies" element={<PoliciesListPage/>}/>

                    <Route path="/semesters/:semesterId/schedules" element={<SemesterSchedulesPage/>}/>
                    <Route path="/schedules/:scheduleId" element={<ScheduleDetailsPage/>}/>

                    <Route element={<RoleRoute allowed={['ADMIN']}/>}>
                        <Route path="/admin/users" element={<UsersListPage/>}/>
                    </Route>
                </Route>
            </Route>

            <Route path="*" element={<Navigate to="/" replace/>}/>
        </Routes>
    );
};
