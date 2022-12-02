import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
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
  constructor(public dialog: MatDialog,public route: ActivatedRoute,private truckService: TruckService,private fb: FormBuilder,private router: Router) { }

  selectedTruck = {
    truckID: "",
    tare: undefined,
    capacity: undefined,
    maxBatteryCapacity: undefined,
    autonomy: undefined,
    fastChargeTime: undefined
  }
  

  ngOnInit(): void {
    const truckID = this.route.snapshot.paramMap.get('id');
    this.formEditTruck = this.fb.group({
      truckID: this.selectedTruck.truckID,
      tare: this.selectedTruck.tare,
      capacity: this.selectedTruck.capacity,
      maxBatteryCapacity: this.selectedTruck.maxBatteryCapacity,
      autonomy: this.selectedTruck.autonomy,
      fastChargeTime: this.selectedTruck.fastChargeTime
    });
    if(truckID)
      this.truckService.getTruck(truckID).then((data) => {
        this.selectedTruck = data;
        this.formEditTruck = this.fb.group({
          truckID: this.selectedTruck.truckID,
          tare: this.selectedTruck.tare,
          capacity: this.selectedTruck.capacity,
          maxBatteryCapacity: this.selectedTruck.maxBatteryCapacity,
          autonomy: this.selectedTruck.autonomy,
          fastChargeTime: this.selectedTruck.fastChargeTime
        });
      });
    else
    this.router.navigate(['Logistics/Home/FleetManager']);
  }


  goBack(){
    this.router.navigate(['Logistics/Home/FleetManager']);
  }
  
  async onSubmit() {
    let answer = await this.truckService.updateTruck(this.formEditTruck.value);
    let message = "Truck updated successfully";
    if(answer.status != 200){
      message = "Error updating truck";
    }
    await this.truckService.updateTruck(answer.json());
    const dialogRef = this.dialog.open(EditTruckComponentDialog, {
      width: '250px',
      data: {
        name: this.selectedTruck.truckID,
        message: message},
    });

    dialogRef.afterClosed().subscribe(result => {
      if(answer.status == 200)
        this.router.navigate(['Logistics/Home/FleetManager']);
      
    });
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



