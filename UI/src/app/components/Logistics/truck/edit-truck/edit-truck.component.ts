import { Component, Inject, OnInit, NgZone } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { TruckService } from 'src/app/Services/TruckService/truck.service';


interface Truck{
  truckID: string;
  tare: number;
  capacity: number;
  maxBatteryCapacity: number;
  autonomy: number;
  fastChargeTime: number;
}

export interface DialogData {
  name: string;
  message: string;
}

@Component({
  selector: 'app-edit-truck',
  templateUrl: './edit-truck.component.html',
  styleUrls: ['./edit-truck.component.css']
})
export class EditTruckComponent implements OnInit {
    formEditTruck!: FormGroup;
  constructor(private ngZone:NgZone,private loginService:LoginService,public dialog: MatDialog,public route: ActivatedRoute,private truckService: TruckService,private fb: FormBuilder,private router: Router) { }

  selectedTruck = {
    truckID: "",
    tare: undefined,
    capacity: undefined,
    maxBatteryCapacity: undefined,
    autonomy: undefined,
    fastChargeTime: undefined
  }


  isAuth: boolean = false;
  authorizedRoles: string[] = ["fltMan","admin"];
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
    const truckID = this.route.snapshot.paramMap.get('id');
    this.formEditTruck = new FormGroup({
      truckID: new FormControl(this.selectedTruck.truckID, [Validators.required]),
      tare: new FormControl(this.selectedTruck.tare, [Validators.required]),
      capacity: new FormControl(this.selectedTruck.capacity, [Validators.required]),
      maxBatteryCapacity: new FormControl(this.selectedTruck.maxBatteryCapacity, [Validators.required]),
      autonomy: new FormControl(this.selectedTruck.autonomy, [Validators.required]),
      fastChargeTime: new FormControl(this.selectedTruck.fastChargeTime, [Validators.required])
    });

    if(truckID)
      this.truckService.getTruck(truckID).then((data) => {
        this.selectedTruck = data;
        this.formEditTruck = new FormGroup({
          truckID: new FormControl(this.selectedTruck.truckID, [Validators.required]),
          tare: new FormControl(this.selectedTruck.tare, [Validators.required]),
          capacity: new FormControl(this.selectedTruck.capacity, [Validators.required]),
          maxBatteryCapacity: new FormControl(this.selectedTruck.maxBatteryCapacity, [Validators.required]),
          autonomy: new FormControl(this.selectedTruck.autonomy, [Validators.required]),
          fastChargeTime: new FormControl(this.selectedTruck.fastChargeTime, [Validators.required])
        });
      });
    else
      this.ngZone.run(() => this.router.navigate(['Logistics/Home/FleetManager']));
  }


  goBack(){
    this.ngZone.run(() => this.router.navigate(['Logistics/Home/FleetManager']));
  }
  
  async onSubmit() {
    if(this.formEditTruck.valid){
      let answer = await this.truckService.updateTruck(this.formEditTruck.value);
      let message = "Truck updated successfully";
      if(answer.status != 200){
        message = "Error updating truck";
      }
      if(answer.status == 200)
          await this.truckService.updateTruckProlog(this.formEditTruck.value);

      const dialogRef = this.dialog.open(EditTruckComponentDialog, {
        width: '250px',
        data: {
          name: this.selectedTruck.truckID,
          message: message},
      });

      dialogRef.afterClosed().subscribe(result => {
        if(answer.status == 200)
          this.ngZone.run(() => this.router.navigate(['Logistics/Home/FleetManager']));
        
      });
    }
  }


}



@Component({
  selector: 'app-edit-truck',
  templateUrl: 'edit-truck.dialog.component.html',
})
  export class EditTruckComponentDialog {
  constructor(
    public dialogRef: MatDialogRef<EditTruckComponentDialog>,
    @Inject(MAT_DIALOG_DATA) public data: DialogData,
  ) {}

  ngOnInit(): void {}

  onOk(): void {
    this.dialogRef.close();
  }
}



