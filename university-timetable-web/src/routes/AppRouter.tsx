import React from 'react';
import {Navigate, Route, Routes} from 'react-router-dom';
import {PublicLayout} from '@/components/layout/PublicLayout';
import {AppLayout} from '@/components/layout/AppLayout';
import {ProtectedRoute} from './ProtectedRoute';
import {RoleRoute} from './RoleRoute';
import {LoginPage} from '@/features/auth/pages/LoginPage';
import {ProfilePage} from '@/features/profile/pages/ProfilePage';
import {AdminDashboardPage} from '@/features/dashboard/pages/AdminDashboardPage';
import {TeacherDashboardPage} from '@/features/dashboard/pages/TeacherDashboardPage';
import {StudentDashboardPage} from '@/features/dashboard/pages/StudentDashboardPage';
import {CoursesListPage} from '@/features/courses/pages/CoursesListPage';
import {GroupsListPage} from '@/features/groups/pages/GroupsListPage';
import {RoomsListPage} from '@/features/rooms/pages/RoomsListPage';
import {SemestersListPage} from '@/features/semesters/pages/SemestersListPage';
import {TeachersListPage} from '@/features/teachers/pages/TeachersListPage';
import {StudentsListPage} from '@/features/students/pages/StudentsListPage';
import {UsersListPage} from '@/features/users/pages/UsersListPage';
import {PoliciesListPage} from '@/features/policies/pages/PoliciesListPage';
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

                    <Route element={<RoleRoute allowed={['ADMIN']}/>}>
                        <Route path="/admin" element={<AdminDashboardPage/>}/>
                        <Route path="/admin/users" element={<UsersListPage/>}/>
                    </Route>

                    <Route element={<RoleRoute allowed={['TEACHER']}/>}>
                        <Route path="/teacher" element={<TeacherDashboardPage/>}/>
                    </Route>

                    <Route element={<RoleRoute allowed={['STUDENT']}/>}>
                        <Route path="/student" element={<StudentDashboardPage/>}/>
                    </Route>
                </Route>
            </Route>

            <Route path="*" element={<Navigate to="/" replace/>}/>
        </Routes>
    );
};
