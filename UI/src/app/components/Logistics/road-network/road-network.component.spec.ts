import { ComponentFixture, TestBed } from '@angular/core/testing';

import { RoadNetworkComponent } from './road-network.component';

describe('RoadNetworkComponent', () => {
  let component: RoadNetworkComponent;
  let fixture: ComponentFixture<RoadNetworkComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ RoadNetworkComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(RoadNetworkComponent);
    component = fixture.componentInstance;
    
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
