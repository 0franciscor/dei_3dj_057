import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TruckPlanningComponent } from './truck-planning.component';

describe('TruckPlanningComponent', () => {
  let component: TruckPlanningComponent;
  let fixture: ComponentFixture<TruckPlanningComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TruckPlanningComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(TruckPlanningComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
