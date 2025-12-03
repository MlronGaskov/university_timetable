import React from "react";
import {Outlet} from "react-router-dom";
import {NavBar} from "../nav/NavBar";
import "./Layout.css";

export const Layout: React.FC = () => {
    return (
        <div className="Layout-root">
            <NavBar/>

            <main className="Layout-main">
                <div className="Layout-content">
                    <Outlet/>
                </div>
            </main>
        </div>
    );
};
