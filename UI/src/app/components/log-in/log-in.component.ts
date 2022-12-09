import { Component, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-log-in',
  templateUrl: './log-in.component.html',
  styleUrls: ['./log-in.component.css']
})
export class LogInComponent implements OnInit {
  hide = true;
  constructor(private loginService:LoginService, private router:Router) { }
  formLogin!: FormGroup;

  isAuth: boolean = true;
  
  async isAuthenticated() {
    const role = await this.loginService.getRole();
    
    if(role=="whMan" ){
      this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']);
      return true;
    }
      
    else if(role=="logMan"){
      this.router.navigate(['/Logistics/Home/LogisticsManager']);
      return true;
    }
      
    else if(role=="fltMan"){
      this.router.navigate(['/Logistics/Home/FleetManager']);
      return true;
    }
    return false
    
  }

  async ngOnInit() {
    this.isAuth = await this.isAuthenticated();
    //TODO: check if admin, if so, redirect to admin page
   
    this.formLogin = new FormGroup({
      userId: new FormControl('', [Validators.required]),
      password: new FormControl('', [Validators.required]),
    });

  }
  
  async onSubmit() {
    if(this.formLogin.valid){
      await this.loginService.login(this.formLogin.value);
      window.location.reload();
      this.router.navigate(['/']);
    }
      
  }

}
