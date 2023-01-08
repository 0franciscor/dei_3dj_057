import { Component, OnInit, NgZone, Inject } from '@angular/core';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from "@angular/material/dialog";
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

    constructor(private ngZone: NgZone, private loginService: LoginService, public dialog: MatDialog, public route: ActivatedRoute, private adminService: AdminService, private router: Router) { }


    isAuth: boolean = false;
    authorizedRoles: string[] = ["fltMan", "logMan", "whMan"];
    async isAuthenticated() {
        const role = await this.loginService.getRole();
        if (!this.authorizedRoles.includes(role)) {
            this.ngZone.run(() => this.router.navigate(['/']));
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

        if (operationSucces) {
            const dialogRef = this.dialog.open(CancelAccountComponentDialog, {
                width: '250px',
            });

            dialogRef.afterClosed().subscribe(result => {
                this.logout();
            });
        }
    }

    logout() {
        this.deleteAllCookies();
        this.ngOnInit();
    }

    deleteAllCookies() {
        const cookies = document.cookie.split(";");

        for (let cookie of cookies) {
            const cookieName = cookie.split("=")[0].trim();
            document.cookie = `${cookieName}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/`
        }
    }

    goBack() {
        this.ngZone.run(() => this.router.navigate(['Admin/Home']));
    }

}


@Component({
    selector: 'app-create-delivery',
    templateUrl: 'cancel-account.dialog.component.html',
})
export class CancelAccountComponentDialog {
    constructor(
        public dialogRef: MatDialogRef<CancelAccountComponentDialog>,
        @Inject(MAT_DIALOG_DATA) public data: DialogData,
    ) { }

    ngOnInit(): void { }

    onOk(): void {
        this.dialogRef.close();
    }
}
