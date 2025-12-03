import React from "react";
import {Routes, Route, Navigate, useLocation} from "react-router-dom";
import {useAuth} from '@/hooks/useAuth';
import {Layout} from "@components/layout/Layout";
import {LoginPage} from "@pages/LoginPage/LoginPage";
import {MainPage} from "@pages/MainPage/MainPage";
import {TimetablePage} from "@pages/TimetablePage/TimetablePage";
import {ProfilePage} from "@pages/ProfilePage/ProfilePage";

import {UsersPage} from "@pages/admin/UsersPage/UsersPage";
import {TeachersPage} from "@pages/admin/TeachersPage/TeachersPage";
import {StudentsPage} from "@pages/admin/StudentsPage/StudentsPage";
import {GroupsPage} from "@pages/admin/GroupsPage/GroupsPage";
import {RoomsPage} from "@pages/admin/RoomsPage/RoomsPage";
import {PoliciesPage} from "@pages/admin/PoliciesPage/PoliciesPage";

import {TeachersCatalogPage} from "@pages/catalog/TeachersCatalogPage/TeachersCatalogPage";
import {StudentsCatalogPage} from "@pages/catalog/StudentsCatalogPage/StudentsCatalogPage";
import {GroupsCatalogPage} from "@pages/catalog/GroupsCatalogPage/GroupsCatalogPage";
import {RoomsCatalogPage} from "@pages/catalog/RoomsCatalogPage/RoomsCatalogPage";

const RequireAuth: React.FC<{ children: React.ReactElement }> = ({children}) => {
    const {isAuthenticated} = useAuth();
    const location = useLocation();

    if (!isAuthenticated) {
        return <Navigate to="/login" state={{from: location}} replace/>;
    }

    return children;
};

const RequireAdmin: React.FC<{ children: React.ReactElement }> = ({children}) => {
    const {role} = useAuth();

    if (role !== 'ADMIN') {
        return <Navigate to="/" replace/>;
    }

    return children;
};

export const App: React.FC = () => {
    const {isAuthenticated} = useAuth();

    return (
        <Routes>
            <Route
                path="/login"
                element={isAuthenticated ? <Navigate to="/" replace/> : <LoginPage/>}
            />

            <Route
                path="/"
                element={
                    <RequireAuth>
                        <Layout/>
                    </RequireAuth>
                }
            >
                <Route index element={<MainPage/>}/>
                <Route path="timetable" element={<TimetablePage/>}/>
                <Route path="profile" element={<ProfilePage/>}/>

                <Route path="teachers" element={<TeachersCatalogPage/>}/>
                <Route path="students" element={<StudentsCatalogPage/>}/>
                <Route path="groups" element={<GroupsCatalogPage/>}/>
                <Route path="rooms" element={<RoomsCatalogPage/>}/>

                <Route
                    path="admin/users"
                    element={
                        <RequireAdmin>
                            <UsersPage/>
                        </RequireAdmin>
                    }
                />
                <Route
                    path="admin/teachers"
                    element={
                        <RequireAdmin>
                            <TeachersPage/>
                        </RequireAdmin>
                    }
                />
                <Route
                    path="admin/students"
                    element={
                        <RequireAdmin>
                            <StudentsPage/>
                        </RequireAdmin>
                    }
                />
                <Route
                    path="admin/groups"
                    element={
                        <RequireAdmin>
                            <GroupsPage/>
                        </RequireAdmin>
                    }
                />
                <Route
                    path="admin/rooms"
                    element={
                        <RequireAdmin>
                            <RoomsPage/>
                        </RequireAdmin>
                    }
                />
                <Route
                    path="admin/policies"
                    element={
                        <RequireAdmin>
                            <PoliciesPage/>
                        </RequireAdmin>
                    }
                />
            </Route>

            <Route
                path="*"
                element={<Navigate to={isAuthenticated ? "/" : "/login"} replace/>}
            />
        </Routes>
    );
};
