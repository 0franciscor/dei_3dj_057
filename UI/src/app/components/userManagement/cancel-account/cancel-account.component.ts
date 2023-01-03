import { Component, OnInit } from '@angular/core';
import { MatDialog } from "@angular/material/dialog";
import { ActivatedRoute, Router } from "@angular/router";
import { AdminService } from "src/app/Services/AdminService/admin.service";
import { LoginService } from "src/app/Services/LoginService/login.service";

export interface DialogData {
    name: string;
    message: string;
}

@Component({
    selector: 'app-cancel-account',
    templateUrl: './cancel-account.component.html',
    styleUrls: ['./cancel-account.component.css']
})
export class CancelAccountComponent implements OnInit {

    public myUser: any;

    constructor(private loginService: LoginService, public dialog: MatDialog, public route: ActivatedRoute, private adminService: AdminService, private router: Router) { }


    isAuth: boolean = false;
    authorizedRoles: string[] = ["user"];
    async isAuthenticated() {
        const role = await this.loginService.getRole();
        if (!this.authorizedRoles.includes(role)) {
            this.router.navigate(['/']);
            return false
        }
        return true;
    }


    async ngOnInit() {
        this.isAuth = await this.isAuthenticated();
        if (this.isAuth) {
            this.adminService.getUser().then((data) => {
                this.myUser = data;
            });
        }

        this.myUser = {
            id: undefined,
            firstName: undefined,
            lastName: undefined,
            email: undefined,
            phoneNumber: undefined,
            password: undefined,
            role: undefined
        }
    }


    encryptUserInfo() {
        this.myUser.firstName = "xxxxxx";
        this.myUser.lastName = "xxxxxx";
        this.myUser.phoneNumber = "xxxxxxxxx";
        this.myUser.role = "deleted";
    }

    async onSubmit() {
        this.encryptUserInfo();
        let operationSucces = await this.adminService.updateUser(this.myUser);
        window.location.reload();
    }

    goBack() {
        this.router.navigate(['Admin/Home']);
    }

}
