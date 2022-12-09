import { Component, OnInit } from '@angular/core';

import { Router } from '@angular/router'
import { LoginService } from 'src/app/Services/LoginService/login.service';

@Component({
  selector: 'app-tool-bar',
  templateUrl: './tool-bar.component.html',
  styleUrls: ['./tool-bar.component.css'] 
})
export class ToolBarComponent implements OnInit {

  constructor(private loginService:LoginService,private router: Router) {}

  

  isAdmn: boolean = false;

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
    if(document.cookie == ""||document.cookie.includes("null")){
      this.router.navigate(['/login']);
    }
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
    window.location.reload();
    this.loginService.logout();
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
 

}
