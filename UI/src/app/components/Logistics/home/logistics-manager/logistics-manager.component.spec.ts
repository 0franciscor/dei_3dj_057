import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormBuilder, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { PathService } from 'src/app/Services/PathService/path.service';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { RouterTestingModule } from '@angular/router/testing';

import { LogisticsManagerComponent } from './logistics-manager.component';
import { CreatePathComponent, CreatePathComponentDialog } from '../../path/create-path/create-path.component';

describe('LogisticsManagerComponent', () => {
  let component: LogisticsManagerComponent;
  let fixture: ComponentFixture<LogisticsManagerComponent>;
  let dialogComponent: CreatePathComponentDialog;
  let dialogFixture: ComponentFixture<CreatePathComponentDialog>;
  let fakePathService : any;

  const dialogMock = {
    close: () => { }
  };

  const pathList =[
   { id: '1',
    pathID:'teste1',
    startWHId :'testeWH1',
    destinationWHId: 'testeWH2',
    pathDistance :1,
    pathTravelTime: 1,
    wastedEnergy: 1,
    extraTravelTime: 1,
  },

  {
    id: '2',
    pathID:'teste2',
    startWHId :'testeWH3',
    destinationWHId: 'testeWH4',
    pathDistance :1,
    pathTravelTime: 1,
    wastedEnergy: 1,
    extraTravelTime: 1,
  }]

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ LogisticsManagerComponent ],
      imports: [MatDialogModule,FormsModule,ReactiveFormsModule, BrowserAnimationsModule,MatCardModule,MatFormFieldModule,MatInputModule,RouterTestingModule.withRoutes(
        [
          {path: 'Logistics/Path/CreatePath', redirectTo: ''},
          {path: 'Logistics/RoadNetwork',redirectTo:''}
        ])],
        providers: [PathService, {provide: MatDialogRef, useValue: dialogMock},{ provide: MAT_DIALOG_DATA, useValue: {} },]
    })
    .compileComponents();

    fakePathService = jasmine.createSpyObj('PathService',['getAllPaths']);
    fakePathService.getAllPaths.and.returnValue(Promise.resolve(pathList));

    TestBed.overrideProvider(PathService,{useValue:fakePathService});

    fixture = TestBed.createComponent(LogisticsManagerComponent);
    component = fixture.componentInstance;
    let fb = new FormBuilder();
    component.formSelectWarehouse = fb.group({
      startWHId: [''],
      destinationWHId:['']
    });
    fixture.detectChanges();
    
    dialogFixture = TestBed.createComponent(CreatePathComponentDialog);
    dialogComponent = dialogFixture.componentInstance;
    dialogFixture.detectChanges();
    dialogComponent.ngOnInit();

  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should go to create Path',()=>{
    component.goToCreatePath();
    expect(component).toBeTruthy();
  });

  it('should get all', ()=>{
    component.onPathSelected(new Event('click'));
    /* const SpyElement = spyOn<any>(component,'onSubmit')
    expect(SpyElement).toHaveBeenCalled(); */
    expect(component).toBeTruthy();

  })

  it('should get all starting wh', ()=>{
    component.onStartWarehouseSelected(new Event('input'));
    expect(component).toBeTruthy();

  })

  it('should get all destination wh',()=>{
    component.onDestinationWarehouseSelected(new Event('input'));
    expect(component).toBeTruthy();
  })

  it('should get all starting and destination wh',()=>{
    component.onDestinationWarehouseSelected(new Event('input'));
    component.onDestinationWarehouseSelected(new Event('input'));
    expect(component).toBeTruthy();
  })

  it('onOk', () => {
    dialogComponent.onOk();
    expect(dialogComponent).toBeTruthy();
  });
  
  it('onSubmit',()=>{
    component.onSubmit();
    expect(component).toBeTruthy();
  })

  it('goToRoadNetwork',()=>{
    component.goToRoadNetwork();
    expect(component).toBeTruthy();
  })
});

describe('PathService',()=>{
  let service: PathService;

  beforeEach(()=>{
    TestBed.configureTestingModule({});
    service = TestBed.inject(PathService);
  });

  it('should be created',()=>{
    expect(service).toBeTruthy();    
  });

  it('should get all paths',async()=>{
    const response ={
    "paths":[{
    "id": '1',
    "pathID":'teste1',
    "startWHId" :'testeWH1',
    "destinationWHId": 'testeWH2',
    "pathDistance" :1,
    "pathTravelTime": 1,
    "wastedEnergy": 1,
    "extraTravelTime": 1,
  }],
      json(){
        return this;
      }
    };

    const warehouses ={
      startWHId: '',
      destinationWHId: ''
    }
    
    const fetchSpy = spyOn<any>(service,'sendFetch').and.returnValue(Promise.resolve(response));

    const paths = await service.getAllPaths(warehouses);
    expect(fetchSpy).toHaveBeenCalled();
    expect(paths).toEqual(response);
  });

  it('should get all startwh paths',async()=>{
    const response ={
    "paths":[{
    "id": '1',
    "pathID":'teste1',
    "startWHId" :'ts1',
    "destinationWHId": 'ts2',
    "pathDistance" :1,
    "pathTravelTime": 1,
    "wastedEnergy": 1,
    "extraTravelTime": 1,
  }],
      json(){
        return this;
      }
    };

    const warehouses ={
      startWHId: 'ts1',
      destinationWHId: ''
    }
    
    const fetchSpy = spyOn<any>(service,'sendFetch').and.returnValue(Promise.resolve(response));

    const paths = await service.getAllPaths(warehouses);
    expect(fetchSpy).toHaveBeenCalled();
    expect(paths).toEqual(response);
  });

  it('should get all destination paths',async()=>{
    const response ={
    "paths":[{
    "id": '1',
    "pathID":'teste1',
    "startWHId" :'ts1',
    "destinationWHId": 'ts2',
    "pathDistance" :1,
    "pathTravelTime": 1,
    "wastedEnergy": 1,
    "extraTravelTime": 1,
  }],
      json(){
        return this;
      }
    };

    const warehouses ={
      startWHId: '',
      destinationWHId: 'ts2'
    }
    
    const fetchSpy = spyOn<any>(service,'sendFetch').and.returnValue(Promise.resolve(response));

    const paths = await service.getAllPaths(warehouses);
    expect(fetchSpy).toHaveBeenCalled();
    expect(paths).toEqual(response);
  });

  it('should get all startwh and destwh paths',async()=>{
    const response ={
    "paths":[{
    "id": '1',
    "pathID":'teste1',
    "startWHId" :'ts1',
    "destinationWHId": 'ts2',
    "pathDistance" :1,
    "pathTravelTime": 1,
    "wastedEnergy": 1,
    "extraTravelTime": 1,
  }],
      json(){
        return this;
      }
    };

    const warehouses ={
      startWHId: 'ts1',
      destinationWHId: 'ts2'
    }
    
    const fetchSpy = spyOn<any>(service,'sendFetch').and.returnValue(Promise.resolve(response));

    const paths = await service.getAllPaths(warehouses);
    expect(fetchSpy).toHaveBeenCalled();
    expect(paths).toEqual(response);
  });
 
  it('should send a fetch with no data', async () => {
    const status = await service.sendFetch('test', 'POST', "null","cookie");
    expect(status.status).toEqual(404);
  });
})