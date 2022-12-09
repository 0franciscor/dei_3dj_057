import { Component, OnInit, ViewChild } from "@angular/core";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { PackagingService } from "src/app/Services/PackageService/package.service";
import { MatPaginator } from "@angular/material/paginator";
import { MatTableDataSource } from "@angular/material/table";

interface Package {
  packagingID: string;
  truckID: string;
  deliveryID: string;
  xPosition: number;
  yPosition: number;
  zPosition: number;
}

@Component({
  selector: 'app-package-list',
  templateUrl: './package-list.component.html',
  styleUrls: ['./package-list.component.css']
})

export class PackageListComponent implements OnInit {

  constructor(private loginService: LoginService,private packageService: PackagingService, private router: Router) {
    this.loadPackages();
  }
  packageList: Package[] = [];

  displayedColumns: string[] = ['packagingID', 'truckID', 'deliveryID', 'xPosition', 'yPosition', 'zPosition', 'edit'];

  dataSource!: MatTableDataSource<Package>;

  @ViewChild('paginator', {static: false})
  set paginator(value: MatPaginator) {
    if(this.dataSource){
      this.dataSource.paginator = value;
    }
  }
  

  loadPackages() {
    this.packageService.getPackage().then((data) => {
      this.packageList = data;
      this.dataSource = new MatTableDataSource(this.packageList);
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