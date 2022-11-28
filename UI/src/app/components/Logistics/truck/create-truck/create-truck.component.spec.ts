import { Injectable } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormGroup, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDialogModule } from '@angular/material/dialog';

import { CreateTruckComponent, CreateTruckComponentDialog } from './create-truck.component';

describe('CreateTruckComponent', () => {
  let component: CreateTruckComponent;
  let dialogComponent : CreateTruckComponentDialog;
  let fixture: ComponentFixture<CreateTruckComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateTruckComponent, CreateTruckComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule],
      providers: [TestService]
    })
    .compileComponents();

    fixture = TestBed.createComponent(CreateTruckComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should create a form with 6 controls', () => {
    expect(component.formCreateTruck.contains('truckID')).toBeTruthy();
    expect(component.formCreateTruck.contains('tare')).toBeTruthy();
    expect(component.formCreateTruck.contains('capacity')).toBeTruthy();
    expect(component.formCreateTruck.contains('maxBatteryCapacity')).toBeTruthy();
    expect(component.formCreateTruck.contains('autonomy')).toBeTruthy();
    expect(component.formCreateTruck.contains('fastChargeTime')).toBeTruthy();
  });

  it('should make the truckID control required', () => {
    let control = component.formCreateTruck.get('truckID');
    control?.setValue('');
    expect(control?.valid).toBeFalsy();
  });

  it('should make the tare control required', () => {
    let control = component.formCreateTruck.get('tare');
    control?.setValue('');
    expect(control?.valid).toBeFalsy();
  });

  it('should make the capacity control required', () => {
    let control = component.formCreateTruck.get('capacity');
    control?.setValue('');
    expect(control?.valid).toBeFalsy();
  });

  it('should make the maxBatteryCapacity control required', () => {
    let control = component.formCreateTruck.get('maxBatteryCapacity');
    control?.setValue('');
    expect(control?.valid).toBeFalsy();
  });

  it('should make the autonomy control required', () => {
    let control = component.formCreateTruck.get('autonomy');
    control?.setValue('');
    expect(control?.valid).toBeFalsy();
  });

  it('should make the fastChargeTime control required', () => {
    let control = component.formCreateTruck.get('fastChargeTime');
    control?.setValue('');
    expect(control?.valid).toBeFalsy();
  });

  it('should create a truck', () => {
    component.formCreateTruck.controls['truckID'].setValue('test');
    component.formCreateTruck.controls['tare'].setValue('test');
    component.formCreateTruck.controls['capacity'].setValue('test');
    component.formCreateTruck.controls['maxBatteryCapacity'].setValue('test');
    component.formCreateTruck.controls['autonomy'].setValue('test');
    component.formCreateTruck.controls['fastChargeTime'].setValue('test');
    expect(component.formCreateTruck.valid).toBeTruthy();
  });

  it('should call the onSubmit method', () => {
    spyOn(component, 'onSubmit');
    let button = fixture.debugElement.nativeElement.querySelector('button');
    button.click();
    expect(component.onSubmit).toHaveBeenCalledTimes(1);
  });


});


@Injectable()
export class TestService {
  constructor() { }
  createTruck(formGroup: FormGroup) { return "--------------------------------------------------------------" }
}