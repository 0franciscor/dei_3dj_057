import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { PackagingService } from "src/app/Services/PackageService/package.service";


@Component({
    selector: 'app-package-list',
    templateUrl: './package-list.component.html',
    styleUrls: ['./package-list.component.css']
  })

  
  export class PackageListComponent implements OnInit {

   public packageList: any[] = [];

   displayedColumns: string[] = ['packagingID','truckID','deliveryID','xPosition','yPosition','zPosition','edit'];
   dataSource = this.packageList;

   constructor(private packageService: PackagingService, private router: Router){

    this.packageService.getPackage().then((data) => {
      this.packageList = data;
      this.dataSource = this.packageList;
    });



   }

  ngOnInit(): void {
  }


  }