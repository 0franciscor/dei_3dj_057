using EletricGo.Domain.Shared;
using EletricGo.Domain.Trucks.ValueObjects;
using EletricGo.Domain.Trucks.DTO;

namespace EletricGo.Domain.Trucks 
{
    public class Truck : Entity<TruckID>
    {

        private TruckID truckID { get; set;}
        private Tare tare { get; set; }
        private Capacity capacity { get; set; }
        private MaxBatteryCapacity maxBatteryCapacity { get; set; }
        private Autonomy autonomy { get; set; }
        private FastChargeTime fastChargeTime { get; set; }

        public Truck(TruckID truckID, Tare tare, Capacity capacity, MaxBatteryCapacity maxBatteryCapacity, Autonomy autonomy, FastChargeTime fastChargeTime)
        {
            this.Id = new TruckID(Guid.NewGuid());
            this.truckID = truckID;
            this.tare = tare;
            this.capacity = capacity;
            this.maxBatteryCapacity = maxBatteryCapacity;
            this.autonomy = autonomy;
            this.fastChargeTime = fastChargeTime;
        }

        public Truck(TruckDTO truckDTO)
        {
            this.truckID = new TruckID(truckDTO.truckID);
            this.tare = new Tare(truckDTO.tare);
            this.capacity = new Capacity(truckDTO.capacity);
            this.maxBatteryCapacity = new MaxBatteryCapacity(truckDTO.maxBatteryCapacity);
            this.autonomy = new Autonomy(truckDTO.autonomy);
            this.fastChargeTime = new FastChargeTime(truckDTO.fastChargeTime);
        }

        public TruckDTO toTruckDTO()
        {
            return new TruckDTO(this.truckID.AsGuid(), this.tare.asFloat(), this.capacity.asFloat(), this.maxBatteryCapacity.asFloat(), this.autonomy.asFloat(), this.fastChargeTime.asFloat());
        }

        public void Update(TruckDTO truckDTO)
        {
            this.truckID = new TruckID(truckDTO.truckID);
            this.tare = new Tare(truckDTO.tare);
            this.capacity = new Capacity(truckDTO.capacity);
            this.maxBatteryCapacity = new MaxBatteryCapacity(truckDTO.maxBatteryCapacity);
            this.autonomy = new Autonomy(truckDTO.autonomy);
            this.fastChargeTime = new FastChargeTime(truckDTO.fastChargeTime);
        }

    }
}


