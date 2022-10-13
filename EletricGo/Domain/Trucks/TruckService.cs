using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Deliveries
{

    public class TruckService
    {
        private readonly ITruckRepository _truckRepository;

        public TruckService(ITruckRepository truckRepository)
        {
            _truckRepository = truckRepository;
        }

        public async Task<List<TruckDTO>> GetTrucks()
        {
            var trucks = await _truckRepository.GetAll();
            return trucks.Select(x => x.ToTruckDTO()).ToList();
        }

        public async Task<TruckDTO> GetTruck(Guid id)
        {
            var truck = await _truckRepository.Get(id);
            return truck.ToTruckDTO();
        }

        public async Task<TruckDTO> CreateTruck(TruckDTO truckDTO)
        {
            var truck = new Truck(truckDTO);
            await _truckRepository.Add(truck);
            return truck.ToTruckDTO();
        }

        public async Task<TruckDTO> UpdateTruck(Guid id, TruckDTO truckDTO)
        {
            var truck = await _truckRepository.Get(id);
            truck.Update(truckDTO);
            await _truckRepository.Update(truck);
            return truck.ToTruckDTO();
        }

        public async Task<TruckDTO> DeleteTruck(Guid id)
        {
            var truck = await _truckRepository.Get(id);
            await _truckRepository.Delete(truck);
            return truck.ToTruckDTO();
        }
    }

}