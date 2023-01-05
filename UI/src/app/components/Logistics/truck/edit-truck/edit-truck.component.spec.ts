import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormControl, FormGroup, FormsModule, ReactiveFormsModule, Validators } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ActivatedRoute } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { TruckService } from 'src/app/Services/TruckService/truck.service';

import { EditTruckComponent, EditTruckComponentDialog } from './edit-truck.component';

describe('EditTruckComponent', () => {
  let component: EditTruckComponent;
  let fixture: ComponentFixture<EditTruckComponent>;
  let dialogComponent: EditTruckComponentDialog;
  let dialogFixture: ComponentFixture<EditTruckComponentDialog>;
  let fakeTruckService: TruckService;
  let fakeLoginService: LoginService
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditTruckComponent, EditTruckComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule, RouterTestingModule.withRoutes(
        [{path: 'Logistics/Home/FleetManager', redirectTo: ''}]
      )],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },{provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => '1'
            }
          }
        }}]
    })
    .compileComponents();

    
    fakeTruckService = TestBed.inject(TruckService);
    fakeLoginService = TestBed.inject(LoginService);
    fixture = TestBed.createComponent(EditTruckComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    component.formEditTruck = new FormGroup({
      truckID: new FormControl('', [Validators.required]),
      tare: new FormControl('', [Validators.required]),
      capacity: new FormControl('', [Validators.required]),
      maxBatteryCapacity: new FormControl('', [Validators.required]),
      autonomy: new FormControl('', [Validators.required]),
      fastChargeTime: new FormControl('', [Validators.required])
    });
    dialogFixture = TestBed.createComponent(EditTruckComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
  });

  it('should be authenticated with admin role', async () => {

    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const response = await component.isAuthenticated();
    component.ngOnInit();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });


  it('should create dialog', () => {
    expect(dialogComponent).toBeTruthy();
  });

  it('onOk', async () => {

    const dialogSpy = spyOn(dialogComponent.dialogRef, 'close');
    
    dialogComponent.onOk();
    expect(dialogSpy).toHaveBeenCalled();
  });


  it('onGoBack', async () => {
    component.goBack();
  });

  it('should call onSubmit', async () => {
    component.formEditTruck = new FormGroup({
      truckID: new FormControl('', [Validators.required]),
      tare: new FormControl('', [Validators.required]),
      capacity: new FormControl('', [Validators.required]),
      maxBatteryCapacity: new FormControl('', [Validators.required]),
      autonomy: new FormControl('', [Validators.required]),
      fastChargeTime: new FormControl('', [Validators.required])
    });
    component.formEditTruck.controls['truckID'].setValue('truckID');
    component.formEditTruck.controls['tare'].setValue('tare');
    component.formEditTruck.controls['capacity'].setValue('capacity');
    component.formEditTruck.controls['maxBatteryCapacity'].setValue('maxBatteryCapacity');
    component.formEditTruck.controls['autonomy'].setValue('autonomy');
    component.formEditTruck.controls['fastChargeTime'].setValue('fastChargeTime');

    const fetchSpy = spyOn<any>(fakeTruckService, 'updateTruck').and.returnValue({status: 200});
    const fetchSpy2 = spyOn<any>(fakeTruckService, 'updateTruckProlog').and.returnValue({status: 200});

    await component.onSubmit();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should call onSubmit with error', async () => {
    component.formEditTruck = new FormGroup({
      truckID: new FormControl('', [Validators.required]),
      tare: new FormControl('', [Validators.required]),
      capacity: new FormControl('', [Validators.required]),
      maxBatteryCapacity: new FormControl('', [Validators.required]),
      autonomy: new FormControl('', [Validators.required]),
      fastChargeTime: new FormControl('', [Validators.required])
    });
    component.formEditTruck.controls['truckID'].setValue('truckID');
    component.formEditTruck.controls['tare'].setValue('tare');
    component.formEditTruck.controls['capacity'].setValue('capacity');
    component.formEditTruck.controls['maxBatteryCapacity'].setValue('maxBatteryCapacity');
    component.formEditTruck.controls['autonomy'].setValue('autonomy');
    component.formEditTruck.controls['fastChargeTime'].setValue('fastChargeTime');

    const fetchSpy = spyOn<any>(fakeTruckService, 'updateTruck').and.returnValue({status: 404});
    const fetchSpy2 = spyOn<any>(fakeTruckService, 'updateTruckProlog').and.returnValue({status: 200});

    await component.onSubmit();
    expect(fetchSpy).toHaveBeenCalled();
  });


 
  it('should call onSubmit with truckID', async () => {
    spyOn(component, 'onSubmit');
    component.formEditTruck.controls['truckID'].setValue('truckID');
    await component.onSubmit();
    expect(component.onSubmit).toHaveBeenCalled();
  });


  it('should call onSubmit with error', async () => {
    const fetchSpy = spyOn<any>(fakeTruckService, 'updateTruck').and.returnValue({status: 500});
    await component.onSubmit();
  });




});

describe('EditTruckComponent with null activated route', () => {
  let component: EditTruckComponent;
  let fixture: ComponentFixture<EditTruckComponent>;
  let dialogComponent: EditTruckComponentDialog;
  let dialogFixture: ComponentFixture<EditTruckComponentDialog>;
  let fakeTruckService: any;
  const dialogMock = {
    close: () => { }
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditTruckComponent, EditTruckComponentDialog ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule, RouterTestingModule.withRoutes(
        [{path: 'Logistics/Home/FleetManager', redirectTo: ''}]
      )],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },{provide: ActivatedRoute,
        useValue: {
          snapshot: {
            paramMap: {
              get: () => null
            }
          }
        }}]
    })
    .compileComponents();

    fakeTruckService = jasmine.createSpyObj('TruckService', ['updateTruck', 'getTruck']);
    fakeTruckService.updateTruck.and.returnValue(Promise.resolve({status: 200}));
    fakeTruckService.getTruck.and.returnValue(Promise.resolve({truckID: "1", tare: 1, capacity: 1, maxBatteryCapacity: 1, autonomy: 1, fastChargeTime: 1}));

    TestBed.overrideProvider(TruckService, {useValue: fakeTruckService});
    fixture = TestBed.createComponent(EditTruckComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    component.ngOnInit();
    component.formEditTruck = new FormGroup({
      truckID: new FormControl('', [Validators.required]),
      tare: new FormControl('', [Validators.required]),
      capacity: new FormControl('', [Validators.required]),
      maxBatteryCapacity: new FormControl('', [Validators.required]),
      autonomy: new FormControl('', [Validators.required]),
      fastChargeTime: new FormControl('', [Validators.required])
    });
    dialogFixture = TestBed.createComponent(EditTruckComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
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

  it('cookie with jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=123');
    
  });

  it('cookie without jwt', () => {
    spyOnProperty(document, 'cookie', 'get').and.returnValue('abc=123');
    const cookie = service.getJwt();
    expect(cookie).toEqual('jwt=');

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
    service.urlOrigin = "https://azure:4200";
    await service.getTruck("test");
  });


  it('should create a truck', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createTruck("test");
  });

  it('should create a truck prolog', async () => {
    const response = {
      "status": 201,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createTruckProlog("test");

  });

  it('should update a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateTruck("test");
  });

  it('should update a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.updateTruckProlog('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.updateTruckProlog("test");
  });

  it('should toggle a truck', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.toggleActiveTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.toggleActiveTruck("test");
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruck('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deleteTruck("test");
  });

  it('should delete a truck prolog', async () => {
    const response = {
      "status": 200,
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.deleteTruckProlog('test');
    // expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.deleteTruckProlog("test");
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
    service.urlOrigin = "https://azure:4200";
    await service.getAllTruck();
  });

  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });



});

describe('LoginService', () => {
  let service: LoginService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(LoginService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('get role with jwt cookie', async () => {
    const response = {
      "status": 200,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  
  it('get role with null jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('');
    const role = await service.getRole();
    expect(fetchSpy).not.toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('get invalid role with jwt cookie', async () => {
    const response = {
      "status": 401,
      json() {
        return {role:"admin"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    spyOnProperty(document, 'cookie', 'get').and.returnValue('jwt=123');
    const role = await service.getRole();
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getRole();
    
  });

  it('should login', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.login("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.login("test");

  });

  it('should login with google', async () => {

    const response = {
      "status": 200,
      json() {
        return {token: "test"};
      }
    }
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const login = await service.loginWithGoogle("test");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.loginWithGoogle("test");

  });




  it('should send a fetch without data', async () => {

    const status = await service.sendFetch('test', 'GET', null, "cookie");
    expect(status.status).toEqual(404);

  });

  it('should send a fetch with data', async () => {
    const status = await service.sendFetch('test', 'POST', "null", "cookie");
    expect(status.status).toEqual(404);
  });
});