import { Component, OnInit } from "@angular/core";
import { FormBuilder, FormControl, FormGroup, Validators } from "@angular/forms";
import { MatDialog } from "@angular/material/dialog";
import { ActivatedRoute, Router } from "@angular/router";
import { StringMatcher } from "cypress/types/net-stubbing";
import { AdminService } from "src/app/Services/AdminService/admin.service";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { LogInComponent } from "../../log-in/log-in.component";
import { AdminHomeComponent } from "../admin-home/admin-home.component";

interface account{
    lastName: string;
    email: string;
    password: string;
    phoneNumber: string;
    role: string;
}

export interface DialogData{
    name: string;
    message: string;
}




@Component({
    selector: 'app-cancel-user',
    templateUrl: './cancel-user.component.html',
    styleUrls: ['./cancel-user.component.css']
})

export class CancelUserComponent implements OnInit {
    formCancelUser!: FormGroup;
    constructor(private loginService:LoginService,public dialog: MatDialog,public route: ActivatedRoute, private adminService:AdminService ,private fb: FormBuilder, private router: Router )
{ }

  selectedUser = {
    firstName: undefined,
    lastName: undefined,
    email: "",
    password: undefined,
    phoneNumber: undefined,
    role: undefined
    
  }



   async ngOnInit(){
        this.isAuth = await this.isAuthenticated();
        const email = this.route.snapshot.paramMap.get('email'); 
        this.formCancelUser= new FormGroup({

            firstName: new FormControl(this.selectedUser.firstName,[Validators.required]),
            lastName: new FormControl(this.selectedUser.lastName,[Validators.required]),
            email: new FormControl(this.selectedUser.email,[Validators.required]),
            password: new FormControl(this.selectedUser.password,[Validators.required]),
            phoneNumber: new FormControl(this.selectedUser.phoneNumber,[Validators.required]),
            role: new FormControl(this.selectedUser.role,[Validators.required])
        });

        console.log(email);

        if(email)
        this.adminService.getAccount(email).then((data) => {
            this.selectedUser = data;
            this.formCancelUser = new FormGroup({
                firstName: new FormControl('xxxxxx'),
                lastName: new FormControl('xxxxxx'),
                email: new FormControl(email),
                password: new FormControl('xxxxxx'),
                phoneNumber: new FormControl('xxxxxx'),
                role: new FormControl('xxxxxx'),
                
            });
        });
        /*
        else
            this.router.navigate(['Admin/Home']);

            */
    }

    goBack(){
        this.router.navigate(['Admin/Home']);
    }

    isAuth: boolean = false;
    authorizedRoles: string[] = ["admin"];
    async isAuthenticated() {
        const role = await this.loginService.getRole();
        if(!this.authorizedRoles.includes(role)){
            this.router.navigate(['/']);
            return false
        }
        else return true;
    }

    async onSubmit() {
        if(this.formCancelUser.valid){
            let answer = await this.adminService.updateUser(this.formCancelUser.value);
            console.log(answer);
            let message = "User canceled";
            if(answer.status != 200) {
                message = "Error canceling user";
            }
        }

        

    }


       

}
 