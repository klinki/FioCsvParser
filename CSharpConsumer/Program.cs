using System;
using FioCsvParser;

namespace CSharpConsumer
{
    class Program
    {
        static void Main(string[] args)
        {
            string filename = @"c:\projects\own\FioCsvParser\FioCsvParser\Data\Obchody.csv";
            var result = CsvParser.ParseFile(filename);
            
            foreach (var item in result)
            {
                Console.WriteLine("%s", item.Tag);
            }

            Console.WriteLine("Hello World!");
        }
    }
}
