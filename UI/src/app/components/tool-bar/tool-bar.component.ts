import { IfStmt } from '@angular/compiler';
import { Component, OnInit } from '@angular/core';

import { Router } from '@angular/router'
import { CookieService } from 'ngx-cookie-service';
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-tool-bar',
  templateUrl: './tool-bar.component.html',
  styleUrls: ['./tool-bar.component.css'] 
})
export class ToolBarComponent implements OnInit {

  constructor(private cookieService:CookieService,private loginService:LoginService,private router: Router) {}

  

  isAdmn: boolean = false;
  isLoggedIn: boolean = false;
  async isAdmin() {
    const role = await this.loginService.getRole();
    if(role=="admin"){
      return true
    }
    else
      return false;
    
  }

  async ngOnInit() {
    this.isAdmn = await this.isAdmin();
    for (let cookie of document.cookie.split(';')) {
      const cookieName = cookie.split("=")[0].trim();
      if(cookieName == "jwt"){
        if(cookie.split("=")[1] == undefined){
          this.isLoggedIn = false;
          this.router.navigate(['/login']);
        }
        else{
          this.isLoggedIn = true;
          break;
        }
      }
    }
    if(!this.isLoggedIn)
      this.router.navigate(['/login']);
  }

  async goHome() {
    //TODO: check if admin, if so, redirect to admin page
    const role = await this.loginService.getRole();
    // if(role == "admin"){
    //   this.router.navigate(['/Admin/Home']);
    // }
    if(role == "whMan"){
      this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']);
    }
    else if(role == "logMan"){
      this.router.navigate(['/Logistics/Home/LogisticsManager']);
    }
    else if(role == "fltMan"){
      this.router.navigate(['/Logistics/Home/FleetManager']);
    }
    else
      this.router.navigate(['/login']);


  }

  logout() {
    const cookies = document.cookie.split(';');

    for (let cookie of cookies) {
      const cookieName = cookie.split("=")[0].trim();
      document.cookie = `${cookieName}=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/`
    }
    window.location.reload();
    this.router.navigate(['/']);
  }


  goToWMHome(): void {
    this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']);
  }

  goToLMHome(): void {
    this.router.navigate(['/Logistics/Home/LogisticsManager']);
  }

  goToFMHome(): void {
    this.router.navigate(['/Logistics/Home/FleetManager']);
  }

  goToTermsAndConditions(): void {
    this.router.navigate(['/TermsAndConditions']);
  }
 

}
