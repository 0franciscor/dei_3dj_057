import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA  } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TruckService } from 'src/app/Services/TruckService/truck.service';

import { CreateTruckComponent, CreateTruckComponentDialog } from './create-truck.component';

describe('CreateTruckComponent', () => {
  let component: CreateTruckComponent;
  let fixture: ComponentFixture<CreateTruckComponent>;
  let dialogComponent: CreateTruckComponentDialog;
  let dialogFixture: ComponentFixture<CreateTruckComponentDialog>;
  let fakeTruckService: any;

  const dialogMock = {
    close: () => { }
    };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ CreateTruckComponent, CreateTruckComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    }).compileComponents();

    fakeTruckService = jasmine.createSpyObj('TruckService', ['createTruck']);
    fakeTruckService.createTruck.and.returnValue(Promise.resolve({status: 201}));

    TestBed.overrideProvider(TruckService, {useValue: fakeTruckService});
    fixture = TestBed.createComponent(CreateTruckComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    
    dialogFixture = TestBed.createComponent(CreateTruckComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
    
    
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('onSubmit with invalid form', async () => {
    component.onSubmit();
    expect(component.formCreateTruck.valid).toBeFalsy();
  });


  it('onSubmit with valid form', async () => {
    component.formCreateTruck.controls['truckID'].setValue('test');
    component.formCreateTruck.controls['tare'].setValue(1);
    component.formCreateTruck.controls['capacity'].setValue(1);
    component.formCreateTruck.controls['maxBatteryCapacity'].setValue(1);
    component.formCreateTruck.controls['autonomy'].setValue(1);
    component.formCreateTruck.controls['fastChargeTime'].setValue(1);
    component.onSubmit();
    expect(component.formCreateTruck.valid).toBeTruthy();
  });


  it('onSubmit with valid form, error on create', async () => {
    component.formCreateTruck.controls['truckID'].setValue('test');
    component.formCreateTruck.controls['tare'].setValue(1);
    component.formCreateTruck.controls['capacity'].setValue(1);
    component.formCreateTruck.controls['maxBatteryCapacity'].setValue(1);
    component.formCreateTruck.controls['autonomy'].setValue(1);
    component.formCreateTruck.controls['fastChargeTime'].setValue(1);
    fakeTruckService.createTruck.and.returnValue(Promise.resolve({status: 500}));
    component.onSubmit();
    expect(component.formCreateTruck.valid).toBeTruthy();
  });

  it('onOk', async () => {
    
    
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });


});


