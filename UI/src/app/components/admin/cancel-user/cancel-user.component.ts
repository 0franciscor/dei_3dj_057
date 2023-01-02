import { Component, OnInit } from "@angular/core";;
import { MatDialog } from "@angular/material/dialog";
import { ActivatedRoute, Router } from "@angular/router";
import { AdminService } from "src/app/Services/AdminService/admin.service";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { ReactiveFormsModule, FormsModule } from "@angular/forms";

export interface DialogData {
    name: string;
    message: string;
}

@Component({
    selector: 'app-cancel-user',
    templateUrl: './cancel-user.component.html',
    styleUrls: ['./cancel-user.component.css']
})

export class CancelUserComponent implements OnInit {

    public accountList: any[] = [];
    public selectedUser: any;
    public selectedUserOption: any;

    constructor(private loginService: LoginService, public dialog: MatDialog, public route: ActivatedRoute, private adminService: AdminService, private router: Router) { }


    isAuth: boolean = false;
    authorizedRoles: string[] = ["admin"];
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
            this.adminService.getAllUsers().then((data) => {
                this.accountList = data;
            });
        }
        this.selectedUser = {
            id: undefined,
            firstName: undefined,
            lastName: undefined,
            email: undefined,
            phoneNumber: undefined,
            password: undefined,
            role: undefined
        }
    }

    onUserSelected($event: any) {
        this.selectedUser = this.accountList.find(element => element.email == this.selectedUserOption);;
    }

    encryptUserInfo() {
        this.selectedUser.firstName = "xxxxxx";
        this.selectedUser.lastName = "xxxxxx";
        this.selectedUser.password = "xxxxxx";
        this.selectedUser.phoneNumber = "xxxxxxxxx";
        this.selectedUser.role = "deleted";
    }

    async onSubmit() {
        this.encryptUserInfo();
        let operationSucces = await this.adminService.updateUser(this.selectedUser);
    }

    goBack() {
        this.router.navigate(['Admin/Home']);
    }

}