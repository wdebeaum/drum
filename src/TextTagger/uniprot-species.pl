# species we prefer to get terms for
our @species = (
  { tp => 'H', # short name to put in terms file after accession number
    OS => qr/^\QHomo sapiens (Human).\E$/, # OS=Organism Species
    OH => qr/^\QNCBI_TaxID=9606; Homo sapiens (Human).\E$/, # OH=Organism Host
    acs => {} # keys = set of accession numbers we got for this species
  },
  { tp => 'M',
    OS => qr/^\QMus musculus (Mouse).\E$/,
    OH => qr/^\QNCBI_TaxID=10090; Mus musculus (Mouse).\E$/,
    acs => {}
  },
  { tp => 'C', # (some) cress
    OS => qr/^Arabidopsis | \(Arabidopsis \w+\)\.$/,
    OH => qr/^\QNCBI_TaxID=\d+; Arabidopsis /,
    acs => {}
  },
  { tp => 'R', # rice
    OS => qr/^Oryza /,
    OH => qr/^\QNCBI_TaxID=\d+; Oryza /,
    acs => {}
  },
  { tp => 'B', # barley
    OS => qr/^Hordeum /,
    OH => qr/^NCBI_TaxID=\d+; Hordeum /,
    acs => {}
  }
);

1;
