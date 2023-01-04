import { Component, OnInit, NgZone } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { AdminService } from 'src/app/Services/AdminService/admin.service';
import { LoginService } from 'src/app/Services/LoginService/login.service';

//roleList interface
export interface Role {
  roleId: string;
  name: string;
}

@Component({
  selector: 'app-create-user',
  templateUrl: './create-user.component.html',
  styleUrls: ['./create-user.component.css']
})
export class CreateUserComponent implements OnInit {
  formCreateUser!: FormGroup;
  constructor(private ngZone:NgZone,private adminService:AdminService,private loginService:LoginService, private router: Router) { }

  roleList: Role[] = []; 

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
    if(this.isAuth){
      this.roleList = await this.adminService.getAllRole();
      this.formCreateUser = new FormGroup({
        firstName: new FormControl('', [Validators.required]),
        lastName: new FormControl('', [Validators.required]),
        email: new FormControl('', [Validators.required]),
        password: new FormControl('', [Validators.required]),
        phoneNumber: new FormControl('', [Validators.required]),
        role: new FormControl('', [Validators.required])
      });
    }
    
  }
  
  isAuth: boolean = false;
  authorizedRoles: string[] = ["admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.ngZone.run(() =>this.router.navigate(['/']));
      return false
    }
    else
      return true;
    
  }

  async onSubmit() {
    if(this.formCreateUser.valid){
      //get this.formCreateUser.value.role in roleList
      this.roleList.forEach(role => {
        if(role.name == this.formCreateUser.value.role){
          this.formCreateUser.value.role = role.roleId;
        }
      });
      

      await this.adminService.createUser(this.formCreateUser.value)
    }
      
  }

}
