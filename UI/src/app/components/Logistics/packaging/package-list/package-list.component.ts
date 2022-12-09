import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
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

   constructor(private loginService:LoginService,private packageService: PackagingService, private router: Router){

    this.packageService.getPackage().then((data) => {
      this.packageList = data;
      this.dataSource = this.packageList;
    });



   }

   isAuth: boolean = false;
  authorizedRoles: string[] = ["logMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.router.navigate(['/']);
      return false
    }
    else
      return true;
    
  }
 
   async ngOnInit() {
     this.isAuth = await this.isAuthenticated();
  }


  }