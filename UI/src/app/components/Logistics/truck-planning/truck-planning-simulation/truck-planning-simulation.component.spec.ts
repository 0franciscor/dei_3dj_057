import { ComponentFixture, TestBed } from "@angular/core/testing";
import { FormsModule, ReactiveFormsModule } from "@angular/forms";
import { MatCardModule } from "@angular/material/card";
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from "@angular/material/dialog";
import { MatFormFieldModule } from "@angular/material/form-field";
import { MatInputModule } from "@angular/material/input";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";
import { LoginService } from "src/app/Services/LoginService/login.service";
import { PlanningService } from "src/app/Services/PlanningService/planning-service.service";
import { TruckPlanningSimulationComponent } from "./truck-planning-simulation.component";

describe('TruckPlanningSimulationComponent', () => {
    let component: TruckPlanningSimulationComponent;
    let fixture: ComponentFixture<TruckPlanningSimulationComponent>;
    let fakeLoginService: LoginService;
    let fakePlanningService: PlanningService;
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
        close:() => { }
    };

    beforeEach(async () => {
        await TestBed.configureTestingModule({
            declarations: [TruckPlanningSimulationComponent],
            imports: [MatDialogModule,FormsModule,ReactiveFormsModule,BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule],
            providers: [PlanningService, {provide: MatDialogRef,useValue: dialogMock},{ provide: MAT_DIALOG_DATA,useValue: {}},]
          }).compileComponents();
        fakeLoginService = TestBed.inject(LoginService);  
        fakePlanningService = TestBed.inject(PlanningService);  
        fixture = TestBed.createComponent(TruckPlanningSimulationComponent);
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

    it('should get best path', async () => {
        
        const fetchSpy = spyOn<any>(fakePlanningService, 'getBestPath').and.returnValue(Promise.resolve(info));
        await component.getBestPath();
        expect(fetchSpy).toHaveBeenCalled();
    });

    it('should get highest mass first path', async () => {
        
        const fetchSpy = spyOn<any>(fakePlanningService, 'getHighestMassFirst').and.returnValue(Promise.resolve(info));
        await component.getHighestMassFirst();
        expect(fetchSpy).toHaveBeenCalled();
    });

    it('should get closest warehouse path', async () => {
        
        const fetchSpy = spyOn<any>(fakePlanningService, 'getClosestWarehouse').and.returnValue(Promise.resolve(info));
        await component.getClosestWarehouse();
        expect(fetchSpy).toHaveBeenCalled();
    });

    it('should get cheapest path', async () => {
        
        const fetchSpy = spyOn<any>(fakePlanningService, 'getCheapestPath').and.returnValue(Promise.resolve(info));
        await component.getCheapestPath();
        expect(fetchSpy).toHaveBeenCalled();
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
  
    // async getBestPath(TruckName: any, date: any){
    //   let url = this.urlOrigin+'api/planning/bestPath'
    //   if(this.urlOrigin.includes("azure")){
    //     url = 'https://auth57.azurewebsites.net/api/packaging/all';
    //   }
    //   const data={
    //     truck: TruckName,
    //     date: date, 
    //   }
  
  
    //   const response = await this.sendFetch(url,'POST',data, this.getJwt());
    //   const pathlist=await response.json();
  
  
     
    //     const url2 = this.urlOrigin + 'api/delivery/getDeliveryDestination'
    //     const body= {pathList:pathlist.bestPath, date:date}
    //     const plan = await this.sendFetch(url2,'POST',body, this.getJwt())
      
      
    //   return plan 
    // }
  
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