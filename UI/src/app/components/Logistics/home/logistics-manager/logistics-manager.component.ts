import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-logistics-manager',
  templateUrl: './logistics-manager.component.html',
  styleUrls: ['./logistics-manager.component.css']
})
export class LogisticsManagerComponent implements OnInit {

  constructor(private router: Router) { }

  ngOnInit(): void {
  }


  goToRoadNetwork() {
    this.router.navigate(['/Logistics/RoadNetwork']);
  }

}
