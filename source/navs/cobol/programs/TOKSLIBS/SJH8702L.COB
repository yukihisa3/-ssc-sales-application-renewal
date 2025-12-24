****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　件数リスト　　　*
*    モジュール名　　　　：　ＤＣＭＪＡＰＡＮ　件数リスト　　　*
*    作成日／更新日　　　：　2007/05/15                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各事業所の件数リストを発行する。　*
*    作成日／更新日　　　：　2017/02/17                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　カーマ（フバ出荷対応）追加　　　　*
*    作成日／更新日　　　：　2018/02/13                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ケーヨー統合追加　　　　　　　　　*
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH8702L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/15.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB-21         IS        CHR-21
     YB            IS        CHR-15
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<ホーマック資材　北海道事業部>>----*
     SELECT   HCSZHOK   ASSIGN         DA-01-S-HCSZHOK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HZH-ST.
*----<<ホーマック資材　東北事業部>>----*
     SELECT   HCSZTOH   ASSIGN         DA-01-S-HCSZTOH
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HZT-ST.
*----<<ホーマック資材　関東事業部>>----*
     SELECT   HCSZKNT   ASSIGN         DA-01-S-HCSZKNT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HZK-ST.
*----<<ホーマック植物　北海道事業部>>----*
     SELECT   HCSKHOK   ASSIGN         DA-01-S-HCSKHOK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKH-ST.
*----<<ホーマック植物　東北事業部>>----*
     SELECT   HCSKTOH   ASSIGN         DA-01-S-HCSKTOH
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKT-ST.
*----<<ホーマック植物　関東事業部>>----*
     SELECT   HCSKKNT   ASSIGN         DA-01-S-HCSKKNT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKK-ST.
*----<<カーマ資材>>----*
     SELECT   KAHMASZ   ASSIGN         DA-01-S-KAHMASZ
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMZ-ST.
*----<<カーマ植物>>----*
     SELECT   KAHMASK   ASSIGN         DA-01-S-KAHMASK
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMK-ST.
*#2017/02/17 NAV ST
*----<<カーマ資材>>----*
     SELECT   KAHMASZ1  ASSIGN         DA-01-S-KAHMASZ1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMZ1-ST.
*----<<カーマ植物>>----*
     SELECT   KAHMASK1  ASSIGN         DA-01-S-KAHMASK1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KMK1-ST.
*#2017/02/17 NAV ED
*----<<ダイキ>>----*
     SELECT   DAIK1     ASSIGN         DA-01-S-DAIK1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI1-ST.
*----<<ダイキ>>----*
     SELECT   DAIK2     ASSIGN         DA-01-S-DAIK2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI2-ST.
*----<<ダイキ>>----*
     SELECT   DAIK3     ASSIGN         DA-01-S-DAIK3
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI3-ST.
*----<<ダイキ>>----*
     SELECT   DAIK4     ASSIGN         DA-01-S-DAIK4
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DI4-ST.
*#2018/02/13 NAV ST
*----<<ケーヨー資材東>>----*
     SELECT   KEIYOSZ1  ASSIGN         DA-01-S-KEIYOSZ1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEZ1-ST.
*----<<ケーヨー資材西>>----*
     SELECT   KEIYOSZ2  ASSIGN         DA-01-S-KEIYOSZ2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KEZ2-ST.
*----<<ケーヨー植物東>>----*
     SELECT   KEIYOSK1  ASSIGN         DA-01-S-KEIYOSK1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KES1-ST.
*----<<ケーヨー資材西>>----*
     SELECT   KEIYOSK2  ASSIGN         DA-01-S-KEIYOSK2
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KES2-ST.
*#2018/02/13 NAV ED
*----<<その他事業部>>----*
     SELECT   ERRORF    ASSIGN         DA-01-S-ERRORF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         ERR-ST.
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN  TO     LP-04-PRTF
                        FILE    STATUS PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<ホーマック資材　北海道事業部>>----*
 FD  HCSZHOK
                        BLOCK CONTAINS 1 RECORDS.
 01  HZH-REC            PIC  X(128).
*----<<ホーマック資材　東北事業部>>----*
 FD  HCSZTOH
                        BLOCK CONTAINS 1 RECORDS.
 01  HZT-REC            PIC  X(128).
*----<<ホーマック資材　関東事業部>>----*
 FD  HCSZKNT
                        BLOCK CONTAINS 1 RECORDS.
 01  HZK-REC            PIC  X(128).
*----<<ホーマック植物　北海道事業部>>----*
 FD  HCSKHOK
                        BLOCK CONTAINS 1 RECORDS.
 01  HKH-REC            PIC  X(128).
*----<<ホーマック植物　東北事業部>>----*
 FD  HCSKTOH
                        BLOCK CONTAINS 1 RECORDS.
 01  HKT-REC            PIC  X(128).
*----<<ホーマック植物　関東事業部>>----*
 FD  HCSKKNT
                        BLOCK CONTAINS 1 RECORDS.
 01  HKK-REC            PIC  X(128).
*----<<カーマ　資材>>----*
 FD  KAHMASZ
                        BLOCK CONTAINS 1 RECORDS.
 01  KMZ-REC            PIC  X(128).
*----<<カーマ　植物>>----*
 FD  KAHMASK
                        BLOCK CONTAINS 1 RECORDS.
 01  KMK-REC            PIC  X(128).
*#2017/02/17 NAV ST
*----<<カーマ　資材>>----*
 FD  KAHMASZ1
                        BLOCK CONTAINS 1 RECORDS.
 01  KMZ1-REC           PIC  X(128).
*----<<カーマ　植物>>----*
 FD  KAHMASK1
                        BLOCK CONTAINS 1 RECORDS.
 01  KMK1-REC           PIC  X(128).
*#2017/02/17 NAV ED
*----<<ダイキ>>----*
 FD  DAIK1
                        BLOCK CONTAINS 1 RECORDS.
 01  DI1-REC     PIC  X(128).
*----<<ダイキ>>----*
 FD  DAIK2
                        BLOCK CONTAINS 1 RECORDS.
 01  DI2-REC            PIC  X(128).
*----<<ダイキ>>----*
 FD  DAIK3
                        BLOCK CONTAINS 1 RECORDS.
 01  DI3-REC     PIC  X(128).
*----<<ダイキ>>----*
 FD  DAIK4
                        BLOCK CONTAINS 1 RECORDS.
 01  DI4-REC            PIC  X(128).
*#2018/02/13 NAV ST
*----<<ケーヨー資材東>>----*
 FD  KEIYOSZ1
                        BLOCK CONTAINS 1 RECORDS.
 01  KEZ1-REC           PIC  X(200).
*----<<ケーヨー資材西>>----*
 FD  KEIYOSZ2
                        BLOCK CONTAINS 1 RECORDS.
 01  KEZ2-REC           PIC  X(200).
*----<<ケーヨー植物東>>----*
 FD  KEIYOSK1
                        BLOCK CONTAINS 1 RECORDS.
 01  KES1-REC           PIC  X(200).
*----<<ケーヨー植物西>>----*
 FD  KEIYOSK2
                        BLOCK CONTAINS 1 RECORDS.
 01  KES2-REC           PIC  X(200).
*#2018/02/13 NAV ED
*----<<その他事業部>>----*
 FD  ERRORF
                        BLOCK CONTAINS 1 RECORDS.
 01  ERR-REC            PIC  X(128).
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  HZH-FLG        PIC  X(03)   VALUE SPACE.
     03  HZT-FLG        PIC  X(03)   VALUE SPACE.
     03  HZK-FLG        PIC  X(03)   VALUE SPACE.
     03  HKH-FLG        PIC  X(03)   VALUE SPACE.
     03  HKT-FLG        PIC  X(03)   VALUE SPACE.
     03  HKK-FLG        PIC  X(03)   VALUE SPACE.
     03  KMZ-FLG        PIC  X(03)   VALUE SPACE.
     03  KMK-FLG        PIC  X(03)   VALUE SPACE.
*#2017/02/17 NAV ST
     03  KMZ1-FLG       PIC  X(03)   VALUE SPACE.
     03  KMK1-FLG       PIC  X(03)   VALUE SPACE.
*#2017/02/17 NAV ED
     03  DI1-FLG        PIC  X(03)   VALUE SPACE.
     03  DI2-FLG        PIC  X(03)   VALUE SPACE.
     03  DI3-FLG        PIC  X(03)   VALUE SPACE.
     03  DI4-FLG        PIC  X(03)   VALUE SPACE.
*#2018/02/13 NAV ST
     03  KEZ1-FLG       PIC  X(03)   VALUE SPACE.
     03  KEZ2-FLG       PIC  X(03)   VALUE SPACE.
     03  KES1-FLG       PIC  X(03)   VALUE SPACE.
     03  KES2-FLG       PIC  X(03)   VALUE SPACE.
*#2018/02/13 NAV ED
     03  ERR-FLG        PIC  X(03)   VALUE SPACE.
     03  CHK-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HZH-CNT        PIC  9(07).
     03  HZT-CNT        PIC  9(07).
     03  HZK-CNT        PIC  9(07).
     03  HKH-CNT        PIC  9(07).
     03  HKT-CNT        PIC  9(07).
     03  HKK-CNT        PIC  9(07).
     03  KMZ-CNT        PIC  9(07).
     03  KMK-CNT        PIC  9(07).
*#2017/02/17 NAV ST
     03  KMZ1-CNT       PIC  9(07).
     03  KMK1-CNT       PIC  9(07).
*#2017/02/17 NAV ED
     03  DI1-CNT        PIC  9(07).
     03  DI2-CNT        PIC  9(07).
     03  DI3-CNT        PIC  9(07).
     03  DI4-CNT        PIC  9(07).
*#2018/02/13 NAV ST
     03  KEZ1-CNT       PIC  9(07).
     03  KEZ2-CNT       PIC  9(07).
     03  KES1-CNT       PIC  9(07).
     03  KES2-CNT       PIC  9(07).
*#2018/02/13 NAV ED
     03  ERR-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HZH-ST         PIC  X(02).
     03  HZT-ST         PIC  X(02).
     03  HZK-ST         PIC  X(02).
     03  HKH-ST         PIC  X(02).
     03  HKT-ST         PIC  X(02).
     03  HKK-ST         PIC  X(02).
     03  KMZ-ST         PIC  X(02).
     03  KMK-ST         PIC  X(02).
*#2017/02/17 NAV ST
     03  KMZ1-ST        PIC  X(02).
     03  KMK1-ST        PIC  X(02).
*#2017/02/17 NAV ED
     03  DI1-ST         PIC  X(02).
     03  DI2-ST         PIC  X(02).
     03  DI3-ST         PIC  X(02).
     03  DI4-ST         PIC  X(02).
*#2018/02/13 NAV ST
     03  KEZ1-ST        PIC  X(02).
     03  KEZ2-ST        PIC  X(02).
     03  KES1-ST        PIC  X(02).
     03  KES2-ST        PIC  X(02).
*#2018/02/13 NAV ED
     03  ERR-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SJH8702L".
 01  WK-MSG1           PIC  N(08)
                       VALUE NC"発注データ有り。".
 01  WK-MSG2           PIC  N(08)
                       VALUE NC"発注データ無し。".
 01  WK-MSG3           PIC  N(15)
                       VALUE NC"振分できない事業所データ有り。".
 01  WK-MSG4           PIC  N(15)
                       VALUE NC"振分できない事業所データ無し。".
 01  WK-MSG5           PIC  N(15)
                       VALUE NC"発注データ有り。（九州分有り）".
*#2017/02/17 NAV ST
 01  WK-MSG6           PIC  N(15)
                       VALUE NC"発注データ有り（カーマ東出荷）".
*#2017/02/17 NAV ED
*#2018/02/14 NAV ST
 01  WK-MSG7           PIC  N(15)
                       VALUE NC"発注データ有（ケーヨー西出荷）".
*#2018/02/14 NAV ED
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(10)  VALUE  SPACE.
     03  HD1-005                 PIC  N(02)
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(23)  VALUE
         NC"※※　ＤＣＭＪＡＰＡＮ事業部別件数リスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"事業部"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(22)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"発注件数"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(04).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(10).
*****03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-03                  PIC  Z,ZZZ,ZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-04                  PIC  N(15).
 01  DT2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT2-01                  PIC  N(14).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
 LINKAGE                SECTION.
 01  PARA-CHK1               PIC X(01).
 01  PARA-CHK2               PIC X(01).
 01  PARA-CHK3               PIC X(01).
 01  PARA-CHK4               PIC X(01).
 01  PARA-CHK5               PIC X(01).
 01  PARA-CHK6               PIC X(01).
 01  PARA-CHK7               PIC X(01).
 01  PARA-CHK8               PIC X(01).
 01  PARA-CHK9               PIC X(01).
 01  PARA-CHK10              PIC X(01).
 01  PARA-CHK11              PIC X(01).
 01  PARA-CHK12              PIC X(01).
 01  PARA-CHK13              PIC X(01).
 01  PARA-CHK14              PIC X(01).
 01  PARA-CHK15              PIC X(01).
 01  PARA-CHK16              PIC X(01).
 01  PARA-CHK17              PIC X(01).
 01  PARA-CHK18              PIC X(01).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-CHK1
                                         PARA-CHK2
                                         PARA-CHK3
                                         PARA-CHK4
                                         PARA-CHK5
                                         PARA-CHK6
                                         PARA-CHK7
                                         PARA-CHK8
                                         PARA-CHK9
                                         PARA-CHK10
                                         PARA-CHK11
                                         PARA-CHK12
                                         PARA-CHK13
                                         PARA-CHK14
                                         PARA-CHK15
                                         PARA-CHK16
                                         PARA-CHK17
                                         PARA-CHK18.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HCSZHOKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSZHOK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L HCSZHOK ERROR " HZH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSZTOHERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSZTOH.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L HCSZTOH ERROR " HZT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSZKNTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSZKNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L HCSZKNT ERROR " HZK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSKHOKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSKHOK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L HCSKHOK ERROR " HKH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSKTOHERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSKTOH.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L HCSKTOH ERROR " HKT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 HCSKKNTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HCSKKNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L HCSKKNT ERROR " HKK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASZERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASZ.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KAHMASZ ERROR " KMZ-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASKERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KAHMASK ERROR " KMK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*#2017/02/17 NAV ST
 KAHMASZ1ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASZ1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KAHMASZ ERROR " KMZ1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KAHMASK1ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KAHMASK1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KAHMASK ERROR " KMK1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*#2017/02/17 NAV ED
 DAIK1ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L DAIK1 ERROR " DI1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 DAIK2ERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DAIK2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L DAIK2 ERROR " DI2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*#2018/02/13 NAV ST
 KEIYOSZ1ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIYOSZ1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KEIYOSZ1 ERROR " KEZ1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KEIYOSZ2ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIYOSZ2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KEIYOSZ2 ERROR " KEZ2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KEIYOSK1ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIYOSK1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KEIYOSK1 ERROR " KES1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 KEIYOSK2ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIYOSK2.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L KEIYOSK2 ERROR " KES2-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*#2018/02/14 NAV ED
 ERRORFERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ERRORF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L ERRORF ERROR " ERR-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PRTFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH8702L PRTFILE ERROR " PRT-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH8702L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HCSZHOK HCSZTOH HCSZKNT KAHMASZ.
     OPEN     INPUT     HCSKHOK HCSKTOH HCSKKNT KAHMASK.
     OPEN     INPUT     DAIK1   DAIK2   ERRORF.
     OPEN     INPUT     DAIK3   DAIK4.
*#2017/02/17 NAV ST
     OPEN     INPUT     KAHMASZ1 KAHMASK1.
*#2017/02/17 NAV ED
*#2018/02/13 NAV ST
     OPEN     INPUT     KEIYOSZ1 KEIYOSZ2.
     OPEN     INPUT     KEIYOSK1 KEIYOSK2.
*#2018/02/13 NAV ED
     OPEN     OUTPUT    PRTFILE.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
*ヘッダ行印字
     PERFORM       HEAD-WT-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*ホーマック資材　北海道事業部件数カウント
*    件数カウント
     PERFORM HCSZHOK-RD-SEC  UNTIL  HZH-FLG = "END".
*    帳票エリアセット
     IF   HZH-CNT  >  ZERO
          MOVE    02             TO  DT1-01
          MOVE NC"ＨＣ資材　北海道"  TO  DT1-02
          MOVE    HZH-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK1
     ELSE
          MOVE    02             TO  DT1-01
          MOVE NC"ＨＣ資材　北海道"  TO  DT1-02
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK1
     END-IF.
*ホーマック資材　東北事業部件数カウント
*    件数カウント
     PERFORM HCSZTOH-RD-SEC  UNTIL  HZT-FLG = "END".
*    帳票エリアセット
     IF   HZT-CNT > ZERO
          MOVE    03             TO  DT1-01
          MOVE NC"ＨＣ資材　東北　"  TO  DT1-02
          MOVE    HZT-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK2
     ELSE
          MOVE    03             TO  DT1-01
          MOVE NC"ＨＣ資材　東北　"  TO  DT1-02
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK2
     END-IF.
*ホーマック資材　関東事業部件数カウント
*    件数カウント
     PERFORM HCSZKNT-RD-SEC  UNTIL  HZK-FLG = "END".
*    帳票エリアセット
     IF   HZK-CNT > ZERO
          MOVE    04             TO  DT1-01
          MOVE NC"ＨＣ資材　関東　"  TO  DT1-02
          MOVE    HZK-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK3
     ELSE
          MOVE    04             TO  DT1-01
          MOVE NC"ＨＣ資材　関東　"  TO  DT1-02
          MOVE    HZK-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK3
     END-IF.
*ホーマック植物　北海道事業部件数カウント
*    件数カウント
     PERFORM HCSKHOK-RD-SEC  UNTIL  HKH-FLG = "END".
*    帳票エリアセット
     IF   HKH-CNT  >  ZERO
          MOVE    02             TO  DT1-01
          MOVE NC"ＨＣ植物　北海道"  TO  DT1-02
          MOVE    HKH-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK5
     ELSE
          MOVE    02             TO  DT1-01
          MOVE NC"ＨＣ植物　北海道"  TO  DT1-02
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK5
     END-IF.
*ホーマック植物　東北事業部件数カウント
*    件数カウント
     PERFORM HCSKTOH-RD-SEC  UNTIL  HKT-FLG = "END".
*    帳票エリアセット
     IF   HKT-CNT > ZERO
          MOVE    03             TO  DT1-01
          MOVE NC"ＨＣ植物　東北　"  TO  DT1-02
          MOVE    HKT-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK6
     ELSE
          MOVE    03             TO  DT1-01
          MOVE NC"ＨＣ植物　東北　"  TO  DT1-02
          MOVE    ZERO           TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK6
     END-IF.
*ホーマック植物　関東事業部件数カウント
*    件数カウント
     PERFORM HCSKKNT-RD-SEC  UNTIL  HKK-FLG = "END".
*    帳票エリアセット
     IF   HKK-CNT > ZERO
          MOVE    04             TO  DT1-01
          MOVE NC"ＨＣ植物　関東　"  TO  DT1-02
          MOVE    HKK-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK7
     ELSE
          MOVE    04             TO  DT1-01
          MOVE NC"ＨＣ植物　関東　"  TO  DT1-02
          MOVE    HKK-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK7
     END-IF.
*カーマ資材　件数カウント
*    件数カウント
     PERFORM KAHMASZ-RD-SEC  UNTIL  KMZ-FLG = "END".
*    帳票エリアセット
     IF   KMZ-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　資材　　"  TO  DT1-02
          MOVE    KMZ-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK4
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　資材　　"  TO  DT1-02
          MOVE    KMZ-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK4
     END-IF.
*カーマ植物　件数カウント
*    件数カウント
     PERFORM KAHMASK-RD-SEC  UNTIL  KMK-FLG = "END".
*    帳票エリアセット
     IF   KMK-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　植物　　"  TO  DT1-02
          MOVE    KMK-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK8
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　植物　　"  TO  DT1-02
          MOVE    KMK-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK8
     END-IF.
*#2017/02/17 NAV ST
*カーマ資材　件数カウント
*    件数カウント
     PERFORM KAHMASZ1-RD-SEC  UNTIL  KMZ1-FLG = "END".
*    帳票エリアセット
     IF   KMZ1-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　資材２　"  TO  DT1-02
          MOVE    KMZ1-CNT       TO  DT1-03
          MOVE    WK-MSG6        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK13
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　資材２　"  TO  DT1-02
          MOVE    KMZ1-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK13
     END-IF.
*カーマ植物　件数カウント
*    件数カウント
     PERFORM KAHMASK1-RD-SEC  UNTIL  KMK1-FLG = "END".
*    帳票エリアセット
     IF   KMK1-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　植物２　"  TO  DT1-02
          MOVE    KMK1-CNT       TO  DT1-03
          MOVE    WK-MSG6        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK14
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"カーマ　植物２　"  TO  DT1-02
          MOVE    KMK1-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK14
     END-IF.
*ダイキ　件数カウント
*    件数カウント
     PERFORM DAIK1-RD-SEC  UNTIL  DI1-FLG = "END".
*    帳票エリアセット
     IF   DI1-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ資材　　　"  TO  DT1-02
          MOVE    DI1-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK9
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ資材　　　"  TO  DT1-02
          MOVE    DI1-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK9
     END-IF.
*ダイキ　件数カウント
*    件数カウント
     PERFORM DAIK2-RD-SEC  UNTIL  DI2-FLG = "END".
*****DISPLAY "DI2-CNT = " DI2-CNT UPON CONS.
*    帳票エリアセット
     IF   DI2-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ植物　　　"  TO  DT1-02
          MOVE    DI2-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK10
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ植物　　　"  TO  DT1-02
          MOVE    DI2-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK10
     END-IF.
*ダイキ　件数カウント
*    件数カウント
     PERFORM DAIK3-RD-SEC  UNTIL  DI3-FLG = "END".
*    帳票エリアセット
     IF   DI3-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ資材：サンコー"  TO  DT1-02
          MOVE    DI3-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK11
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ資材：サンコー"  TO  DT1-02
          MOVE    DI3-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK11
     END-IF.
*ダイキ　件数カウント
*    件数カウント
     PERFORM DAIK4-RD-SEC  UNTIL  DI4-FLG = "END".
*****DISPLAY "DI2-CNT = " DI2-CNT UPON CONS.
*    帳票エリアセット
     IF   DI4-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ植物：サンコー"  TO  DT1-02
          MOVE    DI4-CNT        TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK12
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ダイキ植物：サンコー"  TO  DT1-02
          MOVE    DI4-CNT        TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK12
     END-IF.
*#2018/02/14 NAV ST#############################################
*ケーヨー資材　東　カウント                                    #
*###############################################################
*    件数カウント
     PERFORM KEIYOSZ1-RD-SEC  UNTIL  KEZ1-FLG = "END".
*    帳票エリアセット
     IF   KEZ1-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー資材　東"  TO  DT1-02
          MOVE    KEZ1-CNT       TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK15
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー資材　東"  TO  DT1-02
          MOVE    KEZ1-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK15
     END-IF.
*###############################################################
*ケーヨー資材　西　カウント                                    #
*###############################################################
*    件数カウント
     PERFORM KEIYOSZ2-RD-SEC  UNTIL  KEZ2-FLG = "END".
*    帳票エリアセット
     IF   KEZ2-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー資材　西"  TO  DT1-02
          MOVE    KEZ2-CNT       TO  DT1-03
          MOVE    WK-MSG7        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK16
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー資材　西"  TO  DT1-02
          MOVE    KEZ2-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK16
     END-IF.
*###############################################################
*ケーヨー植物　東　カウント                                    #
*###############################################################
*    件数カウント
     PERFORM KEIYOSK1-RD-SEC  UNTIL  KES1-FLG = "END".
*    帳票エリアセット
     IF   KES1-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー植物　東"  TO  DT1-02
          MOVE    KES1-CNT       TO  DT1-03
          MOVE    WK-MSG1        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK17
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー植物　東"  TO  DT1-02
          MOVE    KES1-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK17
     END-IF.
*###############################################################
*ケーヨー植物　西　カウント                                    #
*###############################################################
*    件数カウント
     PERFORM KEIYOSK2-RD-SEC  UNTIL  KES2-FLG = "END".
*    帳票エリアセット
     IF   KES2-CNT > ZERO
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー植物　西"  TO  DT1-02
          MOVE    KES2-CNT       TO  DT1-03
          MOVE    WK-MSG7        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "1"            TO  PARA-CHK18
     ELSE
          MOVE    ZERO           TO  DT1-01
          MOVE NC"ケーヨー植物　西"  TO  DT1-02
          MOVE    KES2-CNT       TO  DT1-03
          MOVE    WK-MSG2        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
          MOVE    "2"            TO  PARA-CHK18
     END-IF.
*#2018/02/14 NAV ED#############################################
*振り分けエラー件数カウント
*    件数カウント
     PERFORM ERRORF-RD-SEC  UNTIL  ERR-FLG = "END".
*    帳票エリアセット
     IF   ERR-CNT > ZERO
          MOVE    99             TO  DT1-01
          MOVE NC"振分エラー　　　"  TO  DT1-02
          MOVE    ERR-CNT        TO  DT1-03
          MOVE    WK-MSG3        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     ELSE
          MOVE    99             TO  DT1-01
          MOVE NC"振分エラー　　　"  TO  DT1-02
          MOVE    ERR-CNT        TO  DT1-03
          MOVE    WK-MSG4        TO  DT1-04
          WRITE   PRT-REC   FROM   DT1    AFTER  1
          WRITE   PRT-REC   FROM   SEN1   AFTER  1
          MOVE    "CHK"          TO  CHK-FLG
     END-IF.
*    件数チェック
     IF   CHK-FLG = SPACE
          MOVE    WK-MSG2        TO  DT2-01
          WRITE   PRT-REC   FROM   DT2    AFTER  5
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HCSZHOK HCSZTOH HCSZKNT KAHMASZ.
     CLOSE    HCSKHOK HCSKTOH HCSKKNT KAHMASK.
     CLOSE    DAIK1   DAIK2   ERRORF  PRTFILE.
     CLOSE    DAIK3   DAIK4.
*#2017/02/17 NAV ST
     CLOSE    KAHMASZ1  KAHMASK1.
*#2017/02/17 NAV ED
*#2018/02/18 NAV ST
     CLOSE    KEIYOSZ1  KEIYOSZ2.
     CLOSE    KEIYOSK1  KEIYOSK2.
*#2018/02/18 NAV ED

     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH8702L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック資材　北海道データ読込み                 *
*--------------------------------------------------------------*
 HCSZHOK-RD-SEC             SECTION.
     READ   HCSZHOK   AT  END
            MOVE  "END"  TO  HZH-FLG
            NOT   AT  END
            ADD    1     TO  HZH-CNT
     END-READ.
 HCSZHOK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック資材　東北　データ読込み                 *
*--------------------------------------------------------------*
 HCSZTOH-RD-SEC             SECTION.
     READ   HCSZTOH   AT  END
            MOVE  "END"  TO  HZT-FLG
            NOT   AT  END
            ADD    1     TO  HZT-CNT
     END-READ.
 HCSZTOH-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック資材　関東　データ読込み                 *
*--------------------------------------------------------------*
 HCSZKNT-RD-SEC             SECTION.
     READ   HCSZKNT   AT  END
            MOVE  "END"  TO  HZK-FLG
            NOT   AT  END
            ADD    1     TO  HZK-CNT
     END-READ.
 HCSZKNT-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック植物　北海道データ読込み                 *
*--------------------------------------------------------------*
 HCSKHOK-RD-SEC             SECTION.
     READ   HCSKHOK   AT  END
            MOVE  "END"  TO  HKH-FLG
            NOT   AT  END
            ADD    1     TO  HKH-CNT
     END-READ.
 HCSKHOK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック植物　東北　データ読込み                 *
*--------------------------------------------------------------*
 HCSKTOH-RD-SEC             SECTION.
     READ   HCSKTOH   AT  END
            MOVE  "END"  TO  HKT-FLG
            NOT   AT  END
            ADD    1     TO  HKT-CNT
     END-READ.
 HCSKTOH-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ホーマック植物　関東　データ読込み                 *
*--------------------------------------------------------------*
 HCSKKNT-RD-SEC             SECTION.
     READ   HCSKKNT   AT  END
            MOVE  "END"  TO  HKK-FLG
            NOT   AT  END
            ADD    1     TO  HKK-CNT
     END-READ.
 HCSKKNT-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ資材　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASZ-RD-SEC             SECTION.
     READ   KAHMASZ   AT  END
            MOVE  "END"  TO  KMZ-FLG
            NOT   AT  END
            ADD    1     TO  KMZ-CNT
     END-READ.
 KAHMASZ-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ植物　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASK-RD-SEC             SECTION.
     READ   KAHMASK   AT  END
            MOVE  "END"  TO  KMK-FLG
            NOT   AT  END
            ADD    1     TO  KMK-CNT
     END-READ.
 KAHMASK-RD-EXIT.
     EXIT.
*#2017/02/17 NAV ST
*--------------------------------------------------------------*
*           カーマ資材　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASZ1-RD-SEC             SECTION.
     READ   KAHMASZ1   AT  END
            MOVE  "END"  TO  KMZ1-FLG
            NOT   AT  END
            ADD    1     TO  KMZ1-CNT
     END-READ.
 KAHMASZ1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           カーマ植物　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 KAHMASK1-RD-SEC             SECTION.
     READ   KAHMASK1   AT  END
            MOVE  "END"  TO  KMK1-FLG
            NOT   AT  END
            ADD    1     TO  KMK1-CNT
     END-READ.
 KAHMASK1-RD-EXIT.
     EXIT.
*#2017/02/17 NAV ST
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK1-RD-SEC             SECTION.
     READ   DAIK1   AT  END
            MOVE  "END"  TO  DI1-FLG
            NOT   AT  END
            ADD    1     TO  DI1-CNT
     END-READ.
 DAIK1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK2-RD-SEC             SECTION.
     READ   DAIK2   AT  END
            MOVE  "END"  TO  DI2-FLG
            NOT   AT  END
            ADD    1     TO  DI2-CNT
     END-READ.
 DAIK2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK3-RD-SEC             SECTION.
     READ   DAIK3   AT  END
            MOVE  "END"  TO  DI3-FLG
            NOT   AT  END
            ADD    1     TO  DI3-CNT
     END-READ.
 DAIK3-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ダイキ　　　　　　　　データ読込み                 *
*--------------------------------------------------------------*
 DAIK4-RD-SEC             SECTION.
     READ   DAIK4   AT  END
            MOVE  "END"  TO  DI4-FLG
            NOT   AT  END
            ADD    1     TO  DI4-CNT
     END-READ.
 DAIK4-RD-EXIT.
     EXIT.
*#2018/02/14 NAV ST
*--------------------------------------------------------------*
*           ケーヨー資材　東　　　データ読込み                 *
*--------------------------------------------------------------*
 KEIYOSZ1-RD-SEC             SECTION.
     READ   KEIYOSZ1   AT  END
            MOVE  "END"  TO  KEZ1-FLG
            NOT   AT  END
            ADD    1     TO  KEZ1-CNT
     END-READ.
 KEIYOSZ1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ケーヨー資材　西　　　データ読込み                 *
*--------------------------------------------------------------*
 KEIYOSZ2-RD-SEC             SECTION.
     READ   KEIYOSZ2   AT  END
            MOVE  "END"  TO  KEZ2-FLG
            NOT   AT  END
            ADD    1     TO  KEZ2-CNT
     END-READ.
 KEIYOSZ2-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ケーヨー植物　東　　　データ読込み                 *
*--------------------------------------------------------------*
 KEIYOSK1-RD-SEC             SECTION.
     READ   KEIYOSK1   AT  END
            MOVE  "END"  TO  KES1-FLG
            NOT   AT  END
            ADD    1     TO  KES1-CNT
     END-READ.
 KEIYOSK1-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*           ケーヨー植物　西　　　データ読込み                 *
*--------------------------------------------------------------*
 KEIYOSK2-RD-SEC             SECTION.
     READ   KEIYOSK2   AT  END
            MOVE  "END"  TO  KES2-FLG
            NOT   AT  END
            ADD    1     TO  KES2-CNT
     END-READ.
 KEIYOSK2-RD-EXIT.
     EXIT.
*#2018/02/14 NAV ED
*--------------------------------------------------------------*
*                  エラー　データ読込み                        *
*--------------------------------------------------------------*
 ERRORF-RD-SEC             SECTION.
     READ   ERRORF   AT  END
            MOVE  "END"  TO  ERR-FLG
            NOT   AT  END
            ADD    1     TO  ERR-CNT
     END-READ.
 ERRORF-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ■
     MOVE     1                   TO        HD1-04.
***  種別名
*****EVALUATE PARA-KBN
*********WHEN "1"  MOVE  NC"資材" TO        HD1-005
*********WHEN "2"  MOVE  NC"植物" TO        HD1-005
*****END-EVALUATE.

     MOVE     SPACE               TO        HD1-005.

*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  3.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD-WT-EXIT.
     EXIT.
