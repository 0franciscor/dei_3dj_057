import { Component, OnInit } from '@angular/core';

import { Router } from '@angular/router'
@Component({
  selector: 'app-tool-bar',
  templateUrl: './tool-bar.component.html',
  styleUrls: ['./tool-bar.component.css'] 
})
export class ToolBarComponent implements OnInit {

  constructor(private router: Router) { }

  ngOnInit(): void {
  }

  goHome(): void {
    this.router.navigate(['/home']);
  }

  goToFMHome(): void {
    this.router.navigate(['/Logistics/Home/FleetManager']);
  }

  goToLMHome(): void {
    this.router.navigate(['/Logistics/Home/LogisticsManager']);
  }

  goToWMHome(): void {
    this.router.navigate(['/WarehouseManagement/Home/WarehouseManager']);
  }


  

}
