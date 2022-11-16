import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

import{ PathService } from 'src/app/Services/PathService/path.service';

interface path{
  pathID: string;
  startWHId: string;
  destinationWHId: string;
  pathDistance: number;
  pathTravelTime: number;  
  wastedEnergy: number;
  extraTravelTime: number;
};


@Component({
  selector: 'app-logistics-manager',
  templateUrl: './logistics-manager.component.html',
  styleUrls: ['./logistics-manager.component.css']
})
export class LogisticsManagerComponent implements OnInit {

  public selectedPathOption : any;
  public selectedPath: any;

  public pathList: any[]=[];


  constructor(private router: Router,private pathService: PathService) {
    this.pathService.getPath().then((data)=>{
      this.pathList = data;
    });
    this.selectedPath={
      pathID:"",
      startWHId: undefined,
      destinationWHId: undefined,
      pathDistance: undefined,
      pathTravelTime: undefined,
      wastedEnergy: undefined,
      extraTravelTime: undefined,
    }
  }


  ngOnInit(): void {
  }

  onPathSelected($event: any) {
    let test = this.pathList.find(element => element.pathID == this.selectedPathOption);
    console.log(test);
    this.selectedPath = test;
  }

  goToCreatePath(){
    this.router.navigate(['/Logistics/Path/CreatePath']);
  }

  goToRoadNetwork() {
    this.router.navigate(['/Logistics/RoadNetwork']);
  }

}
