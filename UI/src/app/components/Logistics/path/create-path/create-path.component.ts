import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { PathService } from 'src/app/Services/PathService/path.service';

@Component({
  selector: 'app-create-path',
  templateUrl: './create-path.component.html',
  styleUrls: ['./create-path.component.css']
})
export class CreatePathComponent implements OnInit {
 
  formCreatePath!: FormGroup;

  constructor(private pathService: PathService, private fb: FormBuilder) { }

  ngOnInit(): void {
    this.formCreatePath= this.fb.group({
      pathID: [''],
      startWHId: [''],
      destinationWHId: [''],
      pathDistance: [''],
      pathTravelTime: [''],  
      wastedEnergy: [''],
      extraTravelTime:[''],
    });
  }

  onSubmit(){
    this.pathService.createPath(this.formCreatePath.value);
  }

}
