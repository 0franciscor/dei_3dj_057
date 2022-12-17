import { ComponentFixture, TestBed } from '@angular/core/testing';
import { WarehouseService } from 'src/app/Services/WarehouseService/warehouse.service';

import { GetWarehousesComponent } from './get-warehouses.component';

describe('GetWarehousesComponent', () => {
  let component: GetWarehousesComponent;
  let fixture: ComponentFixture<GetWarehousesComponent>;
  let fakeWarehouseService: any;
  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ GetWarehousesComponent ],
      providers: [WarehouseService]
    })
    .compileComponents();
    fakeWarehouseService = jasmine.createSpyObj('WarehouseService', ['getAllWarehouses']);
    fakeWarehouseService.getAllWarehouses.and.returnValue(Promise.resolve({status: 200}));

    TestBed.overrideProvider(WarehouseService, {useValue: fakeWarehouseService});
    fixture = TestBed.createComponent(GetWarehousesComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
