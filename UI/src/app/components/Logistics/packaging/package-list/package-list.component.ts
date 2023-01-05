import { Component, OnInit, ViewChild, NgZone } from "@angular/core";
import { Router } from "@angular/router";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { PackagingService } from "src/app/Services/PackageService/package.service";
import { MatPaginator } from "@angular/material/paginator";
import { MatTableDataSource } from "@angular/material/table";

@Component({
  selector: 'app-package-list',
  templateUrl: './package-list.component.html',
  styleUrls: ['./package-list.component.css']
})

export class PackageListComponent implements OnInit {

  constructor(private ngZone:NgZone,private loginService: LoginService,private packageService: PackagingService, private router: Router) {  }
  originalPackageList: any[] = [];
  packageList: any[] = [];

  displayedColumns: string[] = ['packagingID', 'truckID', 'deliveryID', 'xPosition', 'yPosition', 'zPosition', 'edit'];

  dataSource!: MatTableDataSource<any>;

  @ViewChild('paginator', {static: false})
  set paginator(value: MatPaginator) {
    if(this.dataSource){
      this.dataSource.paginator = value;
    }
  }
  
  isAuth: boolean = false;
  authorizedRoles: string[] = ["logMan","admin"];
  async isAuthenticated() {
    const role= await this.loginService.getRole();
    if(!this.authorizedRoles.includes(role)){
      this.ngZone.run(() => this.router.navigate(['/']));
      return false
    }
    else
      return true;
    
  }

 
  async ngOnInit() {
     this.isAuth = await this.isAuthenticated();
     if(this.isAuth){
      const dataList:any[] = await this.packageService.getPackage();
      this.packageList = dataList;
      this.originalPackageList = dataList.slice();
      this.filteredIDList = dataList.slice();
      this.filteredTruckIDList = dataList.slice();
      this.filteredDeliveryIDList = dataList.slice();
      this.filteredPositionXList = dataList.slice();
      this.filteredPositionYList = dataList.slice();
      this.filteredPositionZList = dataList.slice();
      this.dataSource = new MatTableDataSource(this.packageList);
     }
     
  }

  updateDataSource(packageList: any[] = this.originalPackageList){
    this.dataSource.data = packageList;
    this.packageList = packageList;
  }

  filteredIDList: any[] = [];
  filteredTruckIDList: any[] = [];
  filteredDeliveryIDList: any[] = [];
  filteredPositionXList: any[] = [];
  filteredPositionYList: any[] = [];
  filteredPositionZList: any[] = [];


  applyAllFilters() {

    const filteredPackages = this.originalPackageList.filter(packages =>
      this.filteredIDList.includes(packages) &&
      this.filteredTruckIDList.includes(packages) &&
      this.filteredDeliveryIDList.includes(packages) &&
      this.filteredPositionXList.includes(packages) &&
      this.filteredPositionYList.includes(packages) &&
      this.filteredPositionZList.includes(packages)
      );


    this.updateDataSource(filteredPackages);

  
  }


  
  applyFilterID(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    const filteredPackages = this.originalPackageList.filter((packages) => packages.packagingID.includes(filterValue));
    this.filteredIDList = filteredPackages;

    this.applyAllFilters();
    
  } 
  
  applyFilterTruckID(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    const filteredPackages = this.originalPackageList.filter((packages) => packages.truckID.includes(filterValue));
    this.filteredTruckIDList = filteredPackages;

    this.applyAllFilters();
  }
  
  applyFilterDeliveryID(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    const filteredPackages = this.originalPackageList.filter((packages) => packages.deliveryID.includes(filterValue));
    this.filteredDeliveryIDList = filteredPackages;

    this.applyAllFilters();
  }


  applyFilterPositionX(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    //check if value is empty

    const filteredPackages = this.originalPackageList.filter((packages) => packages.xPosition.toString().includes(filterValue));
    this.filteredPositionXList = filteredPackages;

    this.applyAllFilters();
  } 
  
  applyFilterPositionY(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    const filteredPackages = this.originalPackageList.filter((packages) => packages.yPosition.toString().includes(filterValue));
    this.filteredPositionYList = filteredPackages;
    
    this.applyAllFilters();
  }
  
  applyFilterPositionZ(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    const filteredPackages = this.originalPackageList.filter((packages) => packages.zPosition.toString().includes(filterValue));
    this.filteredPositionZList = filteredPackages;

    this.applyAllFilters();
  }




  

  isIDOrder: number = 0;
  isTruckIDOrder: number = 0;
  isDeliveryIDOrder: number = 0;
  isXPositionOrder: number = 0;
  isYPositionOrder: number = 0;
  isZPositionOrder: number = 0;
  
  orderByID(){
    this.isIDOrder++;
    this.isTruckIDOrder=0;
    this.isDeliveryIDOrder=0;
    this.isXPositionOrder=0;
    this.isYPositionOrder=0;
    this.isZPositionOrder=0;
    if(this.isIDOrder==3)
      this.isIDOrder=0;


    if(this.isIDOrder==1){
      this.packageList.sort((a,b) => a.packagingID.localeCompare(b.packagingID));
      this.updateDataSource();
    }
    else if(this.isIDOrder==2){
      this.packageList.sort((a,b) => b.packagingID.localeCompare(a.packagingID));
      this.updateDataSource();
    }
    else if(this.isIDOrder==0){
      this.packageList=this.originalPackageList.slice();
      this.updateDataSource(this.packageList);
    }
  }
  
  orderByTruckID(){
    this.isIDOrder=0;
    this.isTruckIDOrder++;
    this.isDeliveryIDOrder=0;
    this.isXPositionOrder=0;
    this.isYPositionOrder=0;
    this.isZPositionOrder=0;
    if(this.isTruckIDOrder==3)
      this.isTruckIDOrder=0;

    if(this.isTruckIDOrder==1){
      this.packageList.sort((a,b) => a.truckID.localeCompare(b.truckID));
      this.updateDataSource(this.packageList);
    }
    else if(this.isTruckIDOrder==2){
      this.packageList.sort((a,b) => b.truckID.localeCompare(a.truckID));
      this.updateDataSource(this.packageList);
    }
    else if(this.isTruckIDOrder==0){
      this.packageList=this.originalPackageList.slice();
      this.updateDataSource(this.packageList);
    }


  }

  orderByDeliveryID(){
    this.isIDOrder=0;
    this.isTruckIDOrder=0;
    this.isDeliveryIDOrder++;
    this.isXPositionOrder=0;
    this.isYPositionOrder=0;
    this.isZPositionOrder=0;
    if(this.isDeliveryIDOrder==3)
      this.isDeliveryIDOrder=0;
    
    if(this.isDeliveryIDOrder==1){
      this.packageList.sort((a,b) => a.deliveryID.localeCompare(b.deliveryID));
      this.updateDataSource(this.packageList);
    }
    else if(this.isDeliveryIDOrder==2){
      this.packageList.sort((a,b) => b.deliveryID.localeCompare(a.deliveryID));
      this.updateDataSource(this.packageList);
    }
    else if(this.isDeliveryIDOrder==0){
      this.packageList=this.originalPackageList.slice();
      this.updateDataSource(this.packageList);
    }


  }

  orderByXPosition(){
    this.isIDOrder=0;
    this.isTruckIDOrder=0;
    this.isDeliveryIDOrder=0;
    this.isXPositionOrder++;
    this.isYPositionOrder=0;
    this.isZPositionOrder=0;
    if(this.isXPositionOrder==3)
      this.isXPositionOrder=0;

    if(this.isXPositionOrder==1){
      this.packageList.sort((a,b) => a.xPosition-b.xPosition);
      this.updateDataSource(this.packageList);
    }
    else if(this.isXPositionOrder==2){
      this.packageList.sort((a,b) => b.xPosition-a.xPosition);
      this.updateDataSource(this.packageList);
    }
    else if(this.isXPositionOrder==0){
      this.packageList=this.originalPackageList.slice();
      this.updateDataSource(this.packageList);
    }

  }

  orderByYPosition(){
    this.isIDOrder=0;
    this.isTruckIDOrder=0;
    this.isDeliveryIDOrder=0;
    this.isXPositionOrder=0;
    this.isYPositionOrder++;
    this.isZPositionOrder=0;
    if(this.isYPositionOrder==3)
      this.isYPositionOrder=0;

    if(this.isYPositionOrder==1){
      this.packageList.sort((a,b) => a.yPosition-b.yPosition);
      this.updateDataSource(this.packageList);
    }
    else if(this.isYPositionOrder==2){
      this.packageList.sort((a,b) => b.yPosition-a.yPosition);
      this.updateDataSource(this.packageList);
    }
    else if(this.isYPositionOrder==0){
      this.packageList=this.originalPackageList.slice();
      this.updateDataSource(this.packageList);
    }
        

  }

  orderByZPosition(){
    this.isIDOrder=0;
    this.isTruckIDOrder=0;
    this.isDeliveryIDOrder=0;
    this.isXPositionOrder=0;
    this.isYPositionOrder=0;
    this.isZPositionOrder++;
    if(this.isZPositionOrder==3)
      this.isZPositionOrder=0;
    
    if(this.isZPositionOrder==1){
      this.packageList.sort((a,b) => a.zPosition-b.zPosition);
      this.updateDataSource(this.packageList);
    }
    else if(this.isZPositionOrder==2){
      this.packageList.sort((a,b) => b.zPosition-a.zPosition);
      this.updateDataSource(this.packageList);
    }
    else if(this.isZPositionOrder==0){
      this.packageList=this.originalPackageList.slice();
      this.updateDataSource(this.packageList);
    }



  }

  resetOrder(){
    this.isIDOrder=0;
    this.isTruckIDOrder=0;
    this.isDeliveryIDOrder=0;
    this.isXPositionOrder=0;
    this.isYPositionOrder=0;
    this.isZPositionOrder=0;
    this.packageList=this.originalPackageList.slice();
    this.updateDataSource(this.packageList);
  }
 

}