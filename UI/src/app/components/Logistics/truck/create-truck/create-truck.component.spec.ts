import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA  } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TruckService } from 'src/app/Services/TruckService/truck.service';
import {MatCardModule} from '@angular/material/card';
import {MatFormFieldModule} from '@angular/material/form-field';
import {MatInputModule} from '@angular/material/input';
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
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    }).compileComponents();

    fakeTruckService = jasmine.createSpyObj('TruckService', ['createTruck','createTruckProlog']);
    fakeTruckService.createTruck.and.returnValue(Promise.resolve({status: 201}));

    TestBed.overrideProvider(TruckService, {useValue: fakeTruckService});
    fixture = TestBed.createComponent(CreateTruckComponent);
    component = fixture.componentInstance;
    component.formCreateTruck = new FormGroup({
      truckID: new FormControl('', [Validators.required]),
      tare: new FormControl('', [Validators.required]),
      capacity: new FormControl('', [Validators.required]),
      maxBatteryCapacity: new FormControl('', [Validators.required]),
      autonomy: new FormControl('', [Validators.required]),
      fastChargeTime: new FormControl('', [Validators.required])
    });
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

  it('should create dialog', () => {
    expect(dialogComponent).toBeTruthy();
  });

  it('onSubmit with invalid form', async () => {
    component.formCreateTruck.controls['truckID'].setValue('test');
    await component.onSubmit();
    
    expect(component.formCreateTruck.valid).toBeFalsy();
  });


  it('onSubmit with valid form', async () => {
    component.formCreateTruck.controls['truckID'].setValue('test');
    component.formCreateTruck.controls['tare'].setValue(1);
    component.formCreateTruck.controls['capacity'].setValue(1);
    component.formCreateTruck.controls['maxBatteryCapacity'].setValue(1);
    component.formCreateTruck.controls['autonomy'].setValue(1);
    component.formCreateTruck.controls['fastChargeTime'].setValue(1);
    await component.onSubmit();
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
    await component.onSubmit();
    expect(component.formCreateTruck.valid).toBeTruthy();
  });

  it('onOk', async () => {
    
    
    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });


});

describe('TruckService', () => {
  let service: TruckService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(TruckService);
    


  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

   it('should get a truck', async () => {
    const response = {
      "truckID": "test",
      "tare": 1,
      "capacity": 1,
      "maxBatteryCapacity": 1,
      "autonomy": 1,
      "fastChargeTime": 1,
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const truck = await service.getTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(truck).toEqual(response);
  });


  it('should create a truck', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
  });

  it('should create a truck prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);

  });

  it('should update a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should update a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should delete a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.toggleActiveTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruckProlog('test');
    // expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
  });

  it('should get all trucks', async () => {
    const response = {
      "trucks": [
        {
          "truckID": "test",
          "tare": 1,
          "capacity": 1,
          "maxBatteryCapacity": 1,
          "autonomy": 1,
          "fastChargeTime": 1,
        }
      ],
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const trucks = await service.getAllTruck();
    expect(fetchSpy).toHaveBeenCalled();
    expect(trucks).toEqual(response);
  });

  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null,"cookie");
    expect(status.status).toEqual(404);
  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null","cookie");
    expect(status.status).toEqual(404);
  });

});
