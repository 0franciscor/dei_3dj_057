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
    
    
    public accountList: any[] = [];
    formEmail!: FormGroup;


    constructor(private loginService:LoginService,public dialog: MatDialog,public route: ActivatedRoute, private adminService:AdminService ,private fb: FormBuilder, private router: Router )
{ }

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
        if(this.isAuth){
           
            const email = "";

            this.formEmail= new FormGroup({
                email: new FormControl(this.selectedUser.email,[Validators.required])
                            
            });

           
        }






        /*
        const email = this.route.snapshot.paramMap.get('email'); 
        
        this.formCancelUser= new FormGroup({
            
        
            email: new FormControl(this.selectedUser.email,[Validators.required])
                        
        });

       
        if(email)
        this.adminService.getAccount(email).then((data) => {
            this.selectedUser = data;
            this.formCancelUser.patchValue({
                firstName: ('xxxxxx'),
                lastName: ('xxxxxx'),
                password: ('xxxxxx'),
                phoneNumber: ('xxxxxx'),
                role: ('xxxxxx'),
                
            });
        });
        /*
        else
            this.router.navigate(['Admin/Home']);

            

            console.log(email);

            */
    }

    goBack(){
        this.router.navigate(['Admin/Home']);
    }

    

    async onSubmit() {

        this.adminService.getAllUsers().then((data) => {
            this.accountList = data; 
        });

        
        console.log(this.formEmail.value);

        if(this.formEmail.valid){
            this.selectedUser = this.accountList.find(element => element.email == this.formEmail.value );
            console.log(this.selectedUser)
        }

        

        /* if(this.selectedUser != undefined){
            let answer = await this.adminService.updateUser();
            console.log(answer);
            let message = "User canceled";
            if(answer.status != 200) {
                message = "Error canceling user";
            }
        }

        */

    }


       

}
 