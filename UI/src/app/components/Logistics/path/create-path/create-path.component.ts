import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialog,MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Router } from '@angular/router';
import { PathService } from 'src/app/Services/PathService/path.service';


export interface DialogData{
  name: string
  message: string;
}

@Component({
  selector: 'app-create-path',
  templateUrl: './create-path.component.html',
  styleUrls: ['./create-path.component.css'],
  providers:[PathService]
})
export class CreatePathComponent implements OnInit {
 
  formCreatePath!: FormGroup;

  constructor(public dialog:MatDialog,private pathService: PathService, private fb: FormBuilder,private router: Router) { }

  ngOnInit(): void {
    this.formCreatePath= new FormGroup({
      pathID: new FormControl('', [Validators.required]),
      startWHId: new FormControl('', [Validators.required]),
      destinationWHId: new FormControl('', [Validators.required]),
      pathDistance: new FormControl('', [Validators.required]),
      pathTravelTime: new FormControl('', [Validators.required]),
      wastedEnergy: new FormControl('', [Validators.required]),
      extraTravelTime:new FormControl('', [Validators.required])
    });
   
  }

  async onSubmit(){
    this.formCreatePath.value.pathID ="path"+ this.formCreatePath.value.startWHId + this.formCreatePath.value.destinationWHId
   // if(this.formCreatePath.valid){
     
      let answer = await this.pathService.createPath(this.formCreatePath.value);
      
      let message = "Path created Sucessfully!"
      if (answer.status != 201){
        message = "Error creating Path"
      }
      
      await this.pathService.createPathProlog(this.formCreatePath.value);

      const dialogRef = this.dialog.open(CreatePathComponentDialog,{
        width: '350px',
        data:{
          name: this.formCreatePath.value.startWHId +" -> "+this.formCreatePath.value.destinationWHId,
          message:message
        },
      });

      dialogRef.afterClosed().subscribe(result => {
        if(answer.status == 201){
          this.router.navigate(['Logistics/Home/Logistics Manager']);
        }
      });
    //}
  
  }


  
}

@Component({
  selector :'app-create-path',
  templateUrl : 'create-path.dialog.component.html',
})
export class CreatePathComponentDialog{
  constructor(
    public dialogRef:MatDialogRef<CreatePathComponentDialog>,@Inject(MAT_DIALOG_DATA) public data: DialogData,
  ){}

  ngOnInit(): void{}

  onOk():void{
    this.dialogRef.close();
  }
}
