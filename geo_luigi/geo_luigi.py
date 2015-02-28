import csv
import luigi
import rpy2.robjects.packages as rpackages
workflows = rpackages.importr('geoWorkflows')

class GetGEO(luigi.Task):
    '''Download GEO file 
    '''
    geo_id = luigi.Parameter()
    destdir = luigi.Parameter()
    outputdir = luigi.Parameter() 
    control = luigi.Parameter()
    treatment = luigi.Parameter()
    description = luigi.Parameter()
    
    def output(self):
        return [luigi.LocalTarget(_) for _ in workflows.process_gse(
                geo_id=self.geo_id,
                destdir=self.destdir,
                outputdir=self.outputdir,
                control=list(self.control),
                treatment=list(self.treatment),
                description=self.description
        )]

    def run(self):
        self.output()


class AllDiseases(luigi.Task):
    input_path = luigi.Parameter()
    destdir = luigi.Parameter()
    outputdir = luigi.Parameter() 
 
    fieldnames = ('geo_id', 'disease', 'ctrl_ids', 'pert_ids', 'platform', 'cell_type')


    def process_row(self, row):
        ctrl_ids = row['ctrl_ids'].split(',')
        pert_ids = row['pert_ids'].split(',')
        geo_id = row['geo_id']
        desc = '{0}\t{1}'.format(row['disease'], row['cell_type'])

        if len(ctrl_ids) > 1 and len(pert_ids) > 1:
            return GetGEO(geo_id, self.destdir, self.outputdir, ctrl_ids, pert_ids, desc)


    def requires(self):
        with open(self.input_path) as fr:
            reader = csv.DictReader(fr, delimiter='\t', fieldnames=self.fieldnames)
            rows = [_ for _ in reader]

        return [self.process_row(row) for row in rows]


if __name__ == '__main__':
    luigi.run()
