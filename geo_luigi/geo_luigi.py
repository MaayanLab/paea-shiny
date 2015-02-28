import luigi
import rpy2.robjects.packages as rpackages
workflows = rpackages.importr('geoWorkflows')

class GetGEO(luigi.Task):
    '''Download GEO file 
    '''
    geo_id = luigi.Parameter()
    destdir = luigi.Parameter(None)
    outputdir = luigi.Parameter(None) 
    
    def output(self):
        return [luigi.LocalTarget(_) for _ in workflows.process_gse(
                geo_id=self.geo_id,
                destdir=self.destdir,
                outputdir=self.outputdir)]

    def run(self):
        self.output()


class AllReports(luigi.Task):
    def requires(self):
        return GetGEO('GSE7596', '.', '.')


if __name__ == '__main__':
    luigi.run()
