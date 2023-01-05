import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormControl, FormGroup, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { LoginService } from 'src/app/Services/LoginService/login.service';
import { PlanningService } from 'src/app/Services/PlanningService/planning-service.service';
import { TripService } from 'src/app/Services/TripService/trip.service';
import { TruckService } from 'src/app/Services/TruckService/truck.service';

import { TruckPlanningComponent } from './truck-planning.component';

describe('TruckPlanningComponent', () => {
  let component: TruckPlanningComponent;
  let fixture: ComponentFixture<TruckPlanningComponent>;
  let fakeLoginService: LoginService;
  let fakePlanningService: PlanningService;
  let fakeTripService: TripService;
  const info: any = {
    "date": "2022125",
    "truck": "eTruck01",
    "bestPath": [
        "5",
        "1",
        "3",
        "8",
        "11",
        "9",
        "5"
    ],
    json() {
        return this;
    }
  }
  const dialogMock = {
    close: () => { }
    };
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TruckPlanningComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
      providers: [TruckService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    }).compileComponents();
    fakeLoginService = TestBed.inject(LoginService);  
    fakePlanningService = TestBed.inject(PlanningService);  
    fakeTripService = TestBed.inject(TripService);
    fixture = TestBed.createComponent(TruckPlanningComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should be authenticated with admin role', async () => {

    const fetchSpy = spyOn<any>(fakeLoginService, 'getRole').and.returnValue(Promise.resolve("admin"));
    const response = await component.isAuthenticated();
    expect(response).toBeTrue();
    expect(fetchSpy).toHaveBeenCalled();

  });

  it('should create', () => {
      expect(component).toBeTruthy();
  });

  it('should onSubmit', () => {
    component.formPlanning = new FormGroup({
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    component.onSubmit();
  });
  

  it('should get best path', async () => {
    component.formPlanning = new FormGroup({
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    const fetchSpy = spyOn<any>(fakePlanningService, 'getBestPath').and.returnValue(Promise.resolve(info));
    await component.getBestPath();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should get highest mass first path', async () => {
    component.formPlanning = new FormGroup({
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    const fetchSpy = spyOn<any>(fakePlanningService, 'getHighestMassFirst').and.returnValue(Promise.resolve(info));
    component.getHighestMassFirst();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should get closest warehouse path', async () => {
    component.formPlanning = new FormGroup({
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    const fetchSpy = spyOn<any>(fakePlanningService, 'getClosestWarehouse').and.returnValue(Promise.resolve(info));
    component.getClosestWarehouse();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should get cheapest path', async () => {
    component.formPlanning = new FormGroup({
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    const fetchSpy = spyOn<any>(fakePlanningService, 'getCheapestPath').and.returnValue(Promise.resolve(info));
    component.getCheapestPath();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should get Genetic Algorithm path', async () => {
    component.formPlanning = new FormGroup({
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    const fetchSpy = spyOn<any>(fakePlanningService, 'getGeneticAlgorithm').and.returnValue(Promise.resolve(info));
    component.getGeneticAlgorithm();
    expect(fetchSpy).toHaveBeenCalled();
  });

  it('should go to truck planning simulation', async () => {

    component.getTruckPlanningSimulation();
    expect(component).toBeTruthy();

  });

  it('should format date', async () => {
    //create date object with "Mon Jan 05 2022 00:00:00 GMT+0000 (Western European Standard Time)"
    let date = new Date("Mon Jan 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");

    component.formatDate(date);
    expect(component).toBeTruthy();

  });

  it('should save planning', async () => {
    component.formPlanning = new FormGroup({
      truckName: new FormControl(''),
      planDate: new FormControl(''),
    });
    component.formPlanning.controls['truckName'].setValue("eTruck01");
    component.formPlanning.controls['planDate'].setValue("Mon Dec 05 2022 00:00:00 GMT+0000 (Western European Standard Time)");
    component.finaldate = "20221205"
    component.infoList = info;
    const fetchSpy = spyOn<any>(fakeTripService, 'createTrip').and.returnValue(Promise.resolve({status: 200}));
    component.savePlan();
    expect(component).toBeTruthy();

  });

  it('should go to list truck planning', async () => {

    component.goToListTruckPlanning();
    expect(component).toBeTruthy();

  });


});

describe('PlanningService', () => {
  let service: PlanningService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(PlanningService);
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


  it('should get best path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getBestPath("truck", "date");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getBestPath("truck", "date");
  });

  it('should get highest mass first path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getHighestMassFirst("truck");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getHighestMassFirst("truck");
  });

  it('should get closest warehouse path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getClosestWarehouse("truck");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getClosestWarehouse("truck");

  });

  it('should get cheapest path', async () => {
    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    const path = await service.getCheapestPath("truck");
    expect(fetchSpy).toHaveBeenCalled();
    expect(path.status).toEqual(200);
    service.urlOrigin = "https://azure:4200";
    await service.getCheapestPath("truck");

  });

  it('should get genetic algorithm path', async () => {

    const response = {
      status: 200,
      json: () => Promise.resolve({
        "date": "2022125",
        "truck": "eTruck01",
        "bestPath": [
            "5",
            "1",
            "3",
            "8",
            "11",
            "9",
            "5"
        ]
    })
    };
    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));
    await service.getGeneticAlgorithm("2022125");
    expect(fetchSpy).toHaveBeenCalled();
    service.urlOrigin = "https://azure:4200";
    await service.getGeneticAlgorithm("2022125");



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

describe('TripService', () => {
  let service: TripService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(TripService);
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

  it('should create a trip', async () => {
    const response = {
      "status": 201,
      json() {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const status = await service.createTrip('test');
    expect(fetchSpy).toHaveBeenCalled();
    expect(status.status).toEqual(201);
    service.urlOrigin = "https://azure:4200";
    await service.createTrip("test");
  });

  it('should get all trips', async () => {
    const response = {
      "trip": [
        {
          "tripID": "string",
          "date": "string",
          "pathIDlist": ["string1", "string2"],
          "truckID": "string",
          "deliveryIDlist": ["string1", "string2"]
        }
      ],
      json () {
        return this;
      }
    };

    const fetchSpy = spyOn<any>(service, 'sendFetch').and.returnValue(Promise.resolve(response));

    const trucks = await service.getAllTrips();
    expect(fetchSpy).toHaveBeenCalled();
    expect(trucks).toEqual(response);
    service.urlOrigin = "https://azure:4200";
    await service.getAllTrips();
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

