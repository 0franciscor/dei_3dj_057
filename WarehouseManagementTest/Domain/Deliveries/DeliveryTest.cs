using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using Moq;
using NUnit.Framework;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace WarehouseManagementTest.Domain.Deliveries
{
    public class Tests
    {
        public string Id;
        public int weight;
        public string limitDate;
        public int loadTime;
        public int unloadTime;

        [SetUp]
        public void Setup()
        {
            Id = "Id";
            weight = 10;
            limitDate = "12/12/2012";
            loadTime = 50;
            unloadTime = 70;

        }
        [Test]
        public void DefineDriverServiceConstrutor()
        {

            var mockBlocoRepo = new Mock<IDeliveryRepository>();
            var mockUnitRepo = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitRepo.Object, mockBlocoRepo.Object);

            Assert.NotNull(service);
        }
        [Test]
        public async Task AddTest()
        {
            var del1 = new Delivery(Id, this.weight, this.limitDate, this.unloadTime, this.loadTime);


            var mockRepo = new Mock<IDeliveryRepository>();
            mockRepo.Setup(repo => repo.Add(del1));
            var mockUnitRepo = new Mock<IUnitOfWork>();
            mockUnitRepo.Setup(repo => repo.CommitAsync());

            var service = new DeliveryService(mockUnitRepo.Object, mockRepo.Object);


            var dto = new CreatingDeliveryDto(this.Id, this.weight, this.limitDate, this.unloadTime, this.loadTime);
            var del = await service.AddAsync(dto);

            Assert.AreEqual(del.id.value, del1.Id.value);

        }
        
        [Test]
        public async Task GetByIDTest()
        {
            string IdValue = "Id";

            var idDel = new DeliveryId(IdValue);
            var del = new Delivery(IdValue, this.weight, this.limitDate, this.unloadTime, this.loadTime);

            var mockRepo = new Mock<IDeliveryRepository>();
            mockRepo.Setup(repo => repo.GetByIdAsync(idDel)).ReturnsAsync(del);
            var mockUnitRepo = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitRepo.Object, mockRepo.Object);

            var getDel = await service.GetByIdAsync(idDel);

            Assert.AreEqual(IdValue, getDel.id.value);
        }

        
        [Test]
        public async Task getDeliveriesTest()
        {
            var mockRepo = new Mock<IDeliveryRepository>();
            mockRepo.Setup(repo => repo.GetAllAsync()).ReturnsAsync(createdDeliveries());
            var mockUnitRepo = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitRepo.Object, mockRepo.Object);

            var getDels = await service.GetAllAsync();

            var dels = createdDeliveries();

            Assert.AreEqual(getDels.Count(), dels.Count());

        }

        private List<Delivery> createdDeliveries()
        {
            var dels = new List<Delivery>();
            dels.Add(new Delivery(this.Id, this.weight, this.limitDate, this.unloadTime, this.loadTime));
            dels.Add(new Delivery("Id2", 50, "1/2/2013", 10,30));
            dels.Add(new Delivery("Id3", 100, "15/2/2016", 20,50));
            return dels;
        }


        [Test]
        public async Task UpdateAsyncTest()
        {
            int newWeight = 1000;
            
            string IdValue = "Id";

            var idDel = new DeliveryId(IdValue);
            
            var delDto = new CreatingDeliveryDto(IdValue, newWeight, this.limitDate, this.unloadTime, this.loadTime);
            
            var del = new Delivery(IdValue, this.weight, this.limitDate, this.unloadTime, this.loadTime);

            var mockRepo = new Mock<IDeliveryRepository>();
            mockRepo.Setup(repo => repo.GetByIdAsync(idDel)).ReturnsAsync(del);
            
            var mockUnitRepo = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitRepo.Object, mockRepo.Object);

            var getDel = await service.UpdateAsync(delDto);

            Assert.AreEqual(delDto.weight, getDel.weight.weight);
        }



        [Test]
        public async Task DeleteAsyncTest()
        {
            string IdValue = "Id";

            var idDel = new DeliveryId(IdValue);

            var del = new Delivery(IdValue, this.weight, this.limitDate, this.unloadTime, this.loadTime);

            var mockRepo = new Mock<IDeliveryRepository>();
            mockRepo.Setup(repo => repo.GetByIdAsync(idDel)).ReturnsAsync(del);
            mockRepo.Setup(repo => repo.Remove(del));
            
            var mockUnitRepo = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnitRepo.Object, mockRepo.Object);

            var getDel = await service.DeleteAsync(idDel);

            Assert.AreEqual(IdValue, getDel.id.value);
        }



    }
}