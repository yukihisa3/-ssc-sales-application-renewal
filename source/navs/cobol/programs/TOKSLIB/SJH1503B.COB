****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成　　　　　　　　*
*    作成日／更新日　　　：　99/09/10                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*                            セキチュー用                      *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    2001/03/02 グリーンナンバー１対応追加                     *
*    2002/09/27 支払データ変更　　　　　　　　　　　　　　　　 *
*    2003/02/24 インストアＣＤ_ＪＡＮＣＤ　　　　　　　　　　 *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH1503B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*変換伝票データ
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*出荷場所件数マスタ
     SELECT   JHMKENL1  ASSIGN    TO        DA-01-VI-JHMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JHMTJSL1  ASSIGN    TO        DA-01-VI-JHMTJSL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*検収情報データ
     SELECT   SKENSYUD  ASSIGN    TO        DA-09-S-KENSYUD
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE  STATUS   IS   SKEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払データ
     SELECT   SSIHARAD  ASSIGN    TO        DA-09-S-SIHARAD
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE  STATUS   IS   SSIHA-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*商品情報データ
     SELECT   SSHOHIND  ASSIGN    TO        DA-09-S-SHOHIND
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE  STATUS   IS   SSHO-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２０４８　ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS    16.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  9(02).
         05  DEN-01C             PIC  X(125).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    出荷場所別件数ファイル
******************************************************************
 FD  JHMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JHMTJSL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMTJSF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SKSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
*
*----------------------------------------------------------------*
*    FILE = 検収情報データ        RL=300    BF=3      ORG=SF
*----------------------------------------------------------------*
 FD  SKENSYUD
                        BLOCK CONTAINS      3    RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SKENREC.
 01  SKENREC.
*
     03  SKEN-REC01               PIC  X(128).
     03  SKEN-REC02               PIC  X(128).
     03  SKEN-REC03.
         05  SKEN-REC031          PIC  X(034).
         05  SKEN-REC032          PIC  9(005).
         05  SKEN-REC034          PIC  9(005).
*----------------------------------------------------------------*
*    FILE = 支払情報データ        RL=128    BF=31     ORG=SF
*----------------------------------------------------------------*
 FD  SSIHARAD
                        BLOCK CONTAINS      31   RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SSIHAREC.
 01  SSIHAREC.
*
     03  SSIHA-REC01              PIC  X(128).
*----------------------------------------------------------------*
*    FILE = 商品情報データ        RL=256    BF=4      ORG=SF
*----------------------------------------------------------------*
 FD  SSHOHIND
                        BLOCK CONTAINS      4    RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SSHOREC.
 01  SSHOREC.
*
     03  SSHO-REC01               PIC  X(128).
     03  SSHO-REC02               PIC  X(128).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU1              PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU1              PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  SIHAR-OP-FLG            PIC  9(01)     VALUE  ZERO.
 01  KENSYU-CNT              PIC  9(08)     VALUE  ZERO.
 01  SIHARA-CNT              PIC  9(08)     VALUE  ZERO.
 01  SHOHIN-CNT              PIC  9(08)     VALUE  ZERO.
 01  SKEN-WRT-CNT            PIC  9(08)     VALUE  ZERO.
 01  SSIHA-WRT-CNT           PIC  9(08)     VALUE  ZERO.
 01  SSHO-WRT-CNT            PIC  9(08)     VALUE  ZERO.
*
*データ情報退避ワーク
 01  WK-FL-REC.
     03  WK-FL01                  PIC       X(02).
     03  WK-FL02                  PIC       9(02).
     03  WK-FL03                  PIC       9(06).
     03  WK-FL04                  PIC       9(06).
     03  WK-FL05                  PIC       9(08).
     03  WK-FL06                  PIC       9(06).
     03  WK-FL07                  PIC       9(06).
     03  WK-FL08                  PIC       X(01).
     03  WK-FL09                  PIC       X(20).
     03  WK-FL10.
         05  WK-FL101             PIC       X(20).
         05  WK-FL102             PIC       X(20).
     03  WK-FL11                  PIC       X(08).
     03  WK-FL12                  PIC       9(08).
     03  WK-FL13                  PIC       9(08).
     03  WK-FL14                  PIC       9(01).
     03  WK-FL15                  PIC       9(06).
*
*＜発注．検収ヘッダーＷＫ＞
 01  WK-DENP01A.
     03  WK-DENPA01          PIC  X(01).
     03  WK-DENPA02          PIC  X(02).
     03  WK-DENPA03          PIC  X(09).
     03  WK-DENPA04          PIC  X(04).
     03  WK-DENPA05          PIC  X(05).
     03  WK-DENPA06          PIC  X(04).
     03  WK-DENPA07          PIC  X(02).
     03  WK-DENPA08          PIC  X(01).
     03  WK-DENPA09          PIC  9(06).
     03  WK-DENPA10          PIC  9(06).
     03  WK-DENPA11          PIC  9(06).
     03  WK-DENPA12          PIC  X(06).
     03  WK-DENPA13          PIC  X(02).
     03  WK-DENPA14          PIC  X(15).
     03  WK-DENPA15          PIC  X(15).
     03  WK-DENPA16          PIC  9(06).
     03  WK-DENPA17          PIC  X(09).
     03  WK-DENPA18          PIC  X(15).
     03  WK-DENPA19          PIC  X(14).
*
*＜発注．検収明細ＷＫ＞
 01  WK-DENP01B.
     03  WK-DENPB01          PIC  X(01).
     03  WK-DENPB02          PIC  X(02).
     03  WK-DENPB03          PIC  9(02).
     03  WK-DENPB04          PIC  X(13).
     03  WK-DENPB05          PIC  X(04).
     03  WK-DENPB06          PIC  X(04).
     03  WK-DENPB07          PIC  X(02).
     03  WK-DENPB08          PIC  9(05)V9.
     03  WK-DENPB09          PIC  9(07)V99.
     03  WK-DENPB10          PIC  9(07).
     03  WK-DENPB11          PIC  9(10).
     03  WK-DENPB12          PIC  9(10).
     03  WK-DENPB13          PIC  X(08).
     03  WK-DENPB14          PIC  X(20).
     03  WK-DENPB15          PIC  X(20).
     03  WK-DENPB16          PIC  X(02).
     03  WK-DENPB17          PIC  X(01).
     03  WK-DENPB18          PIC  X(17).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06)      VALUE  ZERO.
     03  SYS-DATEW         PIC 9(08)      VALUE  ZERO.
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02)     VALUE  SPACE.
     03  HEN-STATUS        PIC  X(02)     VALUE  SPACE.
     03  TOK-STATUS        PIC  X(02)     VALUE  SPACE.
     03  TBL-STATUS        PIC  X(02)     VALUE  SPACE.
     03  MEI-STATUS        PIC  X(02)     VALUE  SPACE.
     03  RUT-STATUS        PIC  X(02)     VALUE  SPACE.
     03  KEN-STATUS        PIC  X(02)     VALUE  SPACE.
     03  TJS-STATUS        PIC  X(02)     VALUE  SPACE.
     03  SKEN-STATUS       PIC  X(02)     VALUE  SPACE.
     03  SSIHA-STATUS      PIC  X(02)     VALUE  SPACE.
     03  SSHO-STATUS       PIC  X(02)     VALUE  SPACE.
     03  VLD-STATUS        PIC  X(02)     VALUE  SPACE.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH1503B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH1503B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH1503B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  WK-HENKAN.
     03  WK-TORICD          PIC   X(06).
     03  WK-TORICD-R    REDEFINES WK-TORICD.
       05  WK-TORICD-H      PIC   9(06).
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSHENL1.
     MOVE      "JHSHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMKENL1.
     MOVE      "JHMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMTJSL1.
     MOVE      "JHMTJSL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SKENSYUD.
     MOVE      "SKENSYUD"   TO   AB-FILE.
     MOVE      SKEN-STATUS  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SSIHARAD.
     MOVE      "SSIHARAD"   TO   AB-FILE.
     MOVE      SSIHA-STATUS TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SSHOHIND.
     MOVE      "SSHOHIND"   TO   AB-FILE.
     MOVE      SSHO-STATUS  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CVCSG001
                        SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JHMKENL1  JHMTJSL1.
     OPEN     OUTPUT    SKENSYUD  SSHOHIND  VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DENP01A.
     INITIALIZE                   WK-DENP01A.
     MOVE     SPACE     TO        WK-DENP01B.
     INITIALIZE                   WK-DENP01B.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >         16
*
*ファイルヘッダ対象外
        IF    DEN-01A(IDX)  =     "F"
              CONTINUE
        END-IF
*伝票ヘッダー保管
        IF  ( DEN-01A(IDX)  =     "H"     )  AND
            ( DEN-01B(IDX)  =  01  OR  02 )
*             出荷場所件数マスタ出力
              IF   CNT-MAISU     >    ZERO
              OR   CNT-MAISU1    >    ZERO
                   PERFORM  JHMKENL1-WRT-SEC
              END-IF
              MOVE      SPACE         TO    WK-DENP01A
              INITIALIZE                    WK-DENP01A
              MOVE      DEN-01(IDX)   TO    WK-DENP01A
*2001/03/02 NO.1 START*
**************ADD       1        TO   CNT-MAISU
              EVALUATE  WK-DENPA06
                  WHEN  "0087"   ADD  1     TO  CNT-MAISU1
                  WHEN  OTHER    ADD  1     TO  CNT-MAISU
              END-EVALUATE
*2001/03/02 NO.1 END  *
              MOVE      ZERO     TO   CNT-KENSU-D
              MOVE      ZERO     TO   CNT-GYO
        END-IF
*検収ヘッダー保管
        IF  ( DEN-01A(IDX)  =     "H"     )  AND
            ( DEN-01B(IDX)  =      11     )
              MOVE      SPACE         TO    SKENREC
              MOVE      DEN-01(IDX)   TO    SKEN-REC01
              ADD       1             TO    KENSYU-CNT
        END-IF
*支払ヘッダー保管
        IF  ( DEN-01A(IDX)  =     "H"     )  AND
            ( DEN-01B(IDX)  =      21     )
              IF        SIHAR-OP-FLG  =     ZERO
                        OPEN      OUTPUT    SSIHARAD
                        MOVE  1   TO        SIHAR-OP-FLG
              END-IF
              MOVE      SPACE         TO    SSIHAREC
              MOVE      DEN-01(IDX)   TO    SSIHA-REC01
              PERFORM   SSIHA-WRT-SEC
              ADD       1             TO    SIHARA-CNT
        END-IF
*商品ヘッダー保管
        IF  ( DEN-01A(IDX)  =     "H"     )  AND
            ( DEN-01B(IDX)  =      51     )
              MOVE      SPACE         TO    SSHOREC
              MOVE      DEN-01(IDX)   TO    SSHO-REC01
              ADD       1             TO    SHOHIN-CNT
        END-IF
******
*伝票明細転送
        IF  ( DEN-01A(IDX)  =     "B"     )  AND
            ( DEN-01B(IDX)  =  01  OR  02 )
              MOVE      SPACE         TO    WK-DENP01B
              INITIALIZE                    WK-DENP01B
              MOVE      DEN-01(IDX)   TO    WK-DENP01B
**************ADD       1        TO   CNT-KENSU
              EVALUATE  WK-DENPA06
                  WHEN  "0087"  ADD   1     TO  CNT-KENSU1
                  WHEN  OTHER   ADD   1     TO  CNT-KENSU
              END-EVALUATE
              ADD       1        TO   CNT-KENSU-D
              PERFORM   EDIT-SEC
        END-IF
*検収明細転送
        IF  ( DEN-01A(IDX)  =     "B" )    AND
            ( DEN-01B(IDX)  =      11 )
              MOVE      DEN-01(IDX)   TO   SKEN-REC02
              PERFORM   SKEN-WRT-SEC
        END-IF
*支払明細転送
        IF  ( DEN-01A(IDX)  =     "B" )    AND
            ( DEN-01B(IDX)  =      21 )
              MOVE      DEN-01(IDX)        TO   SSIHA-REC01
              PERFORM   SSIHA-WRT-SEC
**************IF        DEN-01(IDX)(64:60) NOT =    SPACE
**************     MOVE DEN-01(IDX)(64:60) TO   SSIHA-REC02(4:60)
**************     PERFORM  SSIHA-WRT-SEC
**************END-IF
        END-IF
*商品明細転送
        IF  ( DEN-01A(IDX)  =     "B" )    AND
            ( DEN-01B(IDX)  =      51 )
              MOVE      DEN-01(IDX)   TO   SSHO-REC02
              PERFORM   SSHO-WRT-SEC
        END-IF
*
     END-PERFORM.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"      TO   S-NAME.
     MOVE     SPACE          TO   HEN-REC.
     INITIALIZE                   HEN-REC.
*各量販店領域転送
*
     MOVE     WK-DENPA01     TO   HEN-A01.
     MOVE     WK-DENPA02     TO   HEN-A02.
     MOVE     WK-DENPA03     TO   HEN-A03.
     MOVE     WK-DENPA04     TO   HEN-A041.
     MOVE     WK-DENPA05     TO   HEN-A042.
     MOVE     WK-DENPA06     TO   HEN-A05.
     MOVE     WK-DENPA07     TO   HEN-A06.
     MOVE     WK-DENPA09     TO   HEN-A07.
     MOVE     WK-DENPA10     TO   HEN-A08.
     MOVE     WK-DENPA12     TO   HEN-A091.
     MOVE     WK-DENPA13     TO   HEN-A092.
     MOVE     WK-DENPA14     TO   HEN-A101.
     MOVE     "ﾄｸﾊﾝﾌﾞ"       TO   HEN-A102.
     MOVE     WK-DENPA15     TO   HEN-A11.
     MOVE     SPACE          TO   HEN-A12.
     MOVE     SPACE          TO   HEN-A13.
     MOVE     SPACE          TO   HEN-A14.
     MOVE     SPACE          TO   HEN-A151.
     MOVE     SPACE          TO   HEN-A152.
     MOVE     SPACE          TO   HEN-A153.
     MOVE     SPACE          TO   HEN-A154.
     MOVE     SPACE          TO   HEN-A155.
     MOVE     SPACE          TO   HEN-A16.
     MOVE     SPACE          TO   HEN-A17.
     MOVE     "(K.K) ｾｷﾁｭｰ"  TO   HEN-A20.
     MOVE     WK-DENPA18     TO   HEN-A23.
     MOVE     WK-DENPA07     TO   HEN-A41.
     MOVE     WK-DENPA11     TO   HEN-A42.
     MOVE     WK-DENPA13     TO   HEN-A43.
     MOVE     WK-DENPA16     TO   HEN-A44.
*
     MOVE     WK-DENPB01     TO   HEN-A26.
     MOVE     WK-DENPB02     TO   HEN-A27.
     MOVE     WK-DENPB03     TO   HEN-A28.
     MOVE     WK-DENPB13     TO   HEN-A29.
     IF       WK-DENPB05     NOT  NUMERIC
              MOVE     ZERO           TO   HEN-A30
     ELSE
              MOVE     WK-DENPB05     TO   HEN-A30
     END-IF.
     MOVE     ZERO           TO   HEN-A31.
     MOVE     SPACE          TO   HEN-A32.
     MOVE     SPACE          TO   HEN-A33.
     MOVE     WK-DENPB07     TO   HEN-A34.
     MOVE     WK-DENPB08     TO   HEN-A35.
     MOVE     WK-DENPB09     TO   HEN-A36.
     MOVE     WK-DENPB10     TO   HEN-A37.
     MOVE     WK-DENPB11     TO   HEN-A38.
     MOVE     WK-DENPB12     TO   HEN-A39.
     MOVE     WK-DENPB14     TO   HEN-A401.
     MOVE     WK-DENPB15     TO   HEN-A402.
     MOVE     WK-DENPB04     TO   HEN-A403.
     MOVE     SPACE          TO   HEN-A404.
     IF       WK-DENPB06     NOT  NUMERIC
              MOVE     ZERO           TO   HEN-A45
     ELSE
              MOVE     WK-DENPB06     TO   HEN-A45
     END-IF.
*
     PERFORM  TENSO-SEC.
*
     WRITE    HEN-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"     TO   S-NAME.
*取引先コード
     MOVE     WK-DENPA12     TO   WK-TORICD.
     MOVE     WK-TORICD-H    TO   HEN-F01.
*2001/03/02 NO.1 NAV START*
     IF       HEN-A05 = "0087"
              MOVE    7      TO   HEN-F01 WK-TORICD-H
     END-IF.
*2001/03/02 NO.1 NAV END  *
*自社伝票_
     MOVE     HEN-A03        TO   HEN-F02.
     MOVE     HEN-F02        TO   HEN-F23.
*行_
     MOVE     HEN-A28        TO   HEN-F03.
*取区
     MOVE     40             TO   HEN-F051.
     MOVE     NC"売上伝票"   TO   HEN-F052.
*担当者コード
     MOVE     99             TO   HEN-F06.
*店コード
     MOVE     HEN-A042       TO   HEN-F07.
*  商品変換テーブル検索
     MOVE     SPACE          TO   TBL-REC.
     INITIALIZE                   TBL-REC.
     MOVE     HEN-F01        TO   TBL-F01.
*## 2003/02/24 MAV START
*****MOVE     HEN-A29        TO   TBL-F02.
*## インストアＣＤ_ＪＡＮＣＤへ変更
     MOVE     HEN-A403       TO   TBL-F02.
     READ     SHOTBL1
       INVALID
         MOVE      SPACE     TO   TBL-REC
         INITIALIZE               TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE     TBL-F04        TO   HEN-F08
                                  HEN-F09.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
*****COMPUTE  HEN-F111  = WK-NEN * 10000 + HEN-A07.
     MOVE    "3"             TO   LINK-IN-KBN.
     MOVE     HEN-A07        TO   LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE LINK-OUT-YMD8  TO   HEN-F111
     ELSE
              MOVE ZERO           TO   HEN-F111
     END-IF.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
*****COMPUTE  HEN-F112  = WK-NEN * 10000 + HEN-A08.
     MOVE    "3"             TO   LINK-IN-KBN.
     MOVE     HEN-A08        TO   LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING   LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE LINK-OUT-YMD8  TO   HEN-F112
     ELSE
              MOVE ZERO           TO   HEN-F112
     END-IF.
*****MOVE     HEN-F112       TO   HEN-F113.
*分類（部門）
     MOVE     HEN-A05        TO   HEN-F12.
*商品区分　　
     MOVE     HEN-A13        TO   HEN-F131.
*伝票区分　　
     MOVE     HEN-A06        TO   HEN-F132.
*伝発区分
     MOVE     9              TO   HEN-F134.
*自社商品コード
     MOVE     TBL-F031       TO   HEN-F1411.
*自社商品単品コード
     MOVE     TBL-F032       TO   HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE     SPACE          TO   MEI-REC.
     INITIALIZE                   MEI-REC.
     MOVE     TBL-F031       TO   MEI-F011.
     MOVE     TBL-F032       TO   MEI-F012.
     READ     MEIMS1
       INVALID
*        商品名
         MOVE      HEN-A401(1:15) TO   HEN-F1421
         MOVE      HEN-A401(15:5) TO   HEN-F1422(1:5)
         MOVE      HEN-A402(1:10) TO   HEN-F1422(6:10)
       NOT INVALID
*********MOVE      MEI-F031  TO   HEN-F1421
*********MOVE      MEI-F032  TO   HEN-F1422
*## 2000/01/27 NAV START ##
         MOVE      HEN-A401(1:15) TO   HEN-F1421
         MOVE      HEN-A401(15:5) TO   HEN-F1422(1:5)
         MOVE      HEN-A402(1:10) TO   HEN-F1422(6:10)
*## 2000/01/27 NAV END   ##
     END-READ.
*数量
     MOVE     HEN-A35        TO   HEN-F15.
*単
     MOVE    "1"             TO   HEN-F16.
*原価単価
     MOVE     HEN-A36        TO   HEN-F172.
*売価単価
     MOVE     HEN-A37        TO   HEN-F173.
*原価金額
     MOVE     HEN-A38        TO   HEN-F181.
*売価金額
     MOVE     HEN-A39        TO   HEN-F182.
*店舗名（備考）
*****MOVE     HEN-A11        TO   HEN-F22.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      HEN-F01   TO   TOK-F01
         READ      TOKMS2
             INVALID
               MOVE  SPACE   TO   TOK-REC
               INITIALIZE         TOK-REC
         END-READ
         MOVE    TOK-F52     TO   HEN-F24
         MOVE    1           TO   FLG-TOK
     ELSE
         MOVE    TOK-F52     TO   HEN-F24
     END-IF.
*相手商品コード
*****MOVE     HEN-A29        TO   HEN-F25.
*インストア_ＪＡＮＣＤヘ変更
     MOVE     HEN-A403       TO   HEN-F25.
*伝票発行区分
     MOVE     9              TO   HEN-F272.
*オンライン区分
     MOVE     1              TO   HEN-F274.
*エントリー区分
     MOVE     1              TO   HEN-F275.
*付番区分
     MOVE     9              TO   HEN-F276.
*量販店区分
*****MOVE    "F"             TO   HEN-F278.
     MOVE    "A"             TO   HEN-F278.
*ＷＳ_
     MOVE     1              TO   HEN-F28.
*変換値
*****MOVE     WK-NEN         TO   HEN-F29.
*店舗名（カナ）
     MOVE     HEN-A11        TO   HEN-F30.
*システム日付
     MOVE     SYS-DATEW      TO   HEN-F99.
*受信日付
     MOVE     PARA-JDATE     TO   HEN-F46.
*受信時刻
     MOVE     PARA-JTIME     TO   HEN-F47.
*振分倉庫コード
*    ルート条件マスタ検索
     MOVE     ZERO           TO   INV-RUT.
     MOVE     SPACE          TO   RUT-REC.
     INITIALIZE                   RUT-REC.
     MOVE     HEN-F01        TO   RUT-F01.
     MOVE     SPACE          TO   RUT-F02.
     MOVE     HEN-A43        TO   RUT-F03  HEN-F42.
     READ     JHMRUTL1
         INVALID
           MOVE    1         TO   INV-RUT
           MOVE    TOK-F81   TO   HEN-F48
         NOT INVALID
           MOVE    RUT-F05   TO   HEN-F48
     END-READ.
*_番
     MOVE     TBL-F08        TO   HEN-F49.
*訂正前数量
     MOVE     HEN-A35        TO   HEN-F50.
*修正原価単価
     MOVE     HEN-A36        TO   HEN-F512.
*修正売価単価
     MOVE     HEN-A37        TO   HEN-F513.
*修正原価金額
     MOVE     HEN-A38        TO   HEN-F521.
*修正売価金額
     MOVE     HEN-A39        TO   HEN-F522.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JHMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JHMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     READ    JHMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JHMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JHMKENL1-WRT-EXIT.
*
 JHMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D   TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JHMKENL1-WRT-EXIT.
     EXIT.
******************************************************************
*             ﾚｺ-ﾄﾞ ﾗｲﾄ   (ｶｷｺﾐ)   ｼﾖﾘ    検収情報データ
******************************************************************
 SKEN-WRT-SEC      SECTION.
*
     MOVE    "SKEN-WRT-SEC"  TO   S-NAME.
     WRITE    SKENREC.
     IF       SKEN-STATUS = "34"
              DISPLAY   "*** ｹﾝｼｭｳﾃﾞｰﾀ( SKENSYUD ) ｵｰﾊﾞｰﾌﾛｰ ***"
                                 UPON  CONS
**            ACCEPT    AAA      FROM  X-STATION
              STOP      RUN
     END-IF.
     ADD      1         TO       SKEN-WRT-CNT.
*
 SKEN-WRT-EXIT.
     EXIT.
******************************************************************
*             ﾚｺ-ﾄﾞ ﾗｲﾄ   (ｶｷｺﾐ)   ｼﾖﾘ    支払情報データ
******************************************************************
 SSIHA-WRT-SEC     SECTION.
*
     MOVE    "SSIHA-WRT-SEC"  TO   S-NAME.
     WRITE    SSIHAREC.
     IF       SSIHA-STATUS = "34"
              DISPLAY   "*** ｼﾊﾗｲﾃﾞｰﾀ( SSIHARAD) ｵｰﾊﾞｰﾌﾛｰ ***"
                                  UPON  CONS
**            ACCEPT    AAA       FROM  X-STATION
              STOP      RUN
     END-IF.
     ADD      1         TO        SSIHA-WRT-CNT.
*
 SSIHA-WRT-EXIT.
     EXIT.
******************************************************************
*             ﾚｺ-ﾄﾞ ﾗｲﾄ   (ｶｷｺﾐ)   ｼﾖﾘ    商品情報データ
******************************************************************
 SSHO-WRT-SEC      SECTION.
*
     MOVE    "SSHO-WRT-SEC"  TO   S-NAME.
     WRITE    SSHOREC.
     IF       SSHO-STATUS = "34"
              DISPLAY   "*** ｼｮｳﾋﾝﾃﾞｰﾀ( SSHOHIND ) ｵｰﾊﾞｰﾌﾛｰ ***"
                                  UPON  CONS
**            ACCEPT    AAA       FROM  X-STATION
              STOP      RUN
     END-IF.
     ADD      1         TO        SSHO-WRT-CNT.
*
 SSHO-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF        CNT-MAISU     >    ZERO
     AND       WK-TORICD-H   =    321717
               MOVE     321717    TO    HEN-F01
*              出荷場所件数マスタ出力
               PERFORM  JHMKENL1-WRT-SEC
     END-IF.
*
     IF        CNT-MAISU1    >    ZERO
     AND       WK-TORICD-H   =    7
               MOVE     7         TO    HEN-F01
*              出荷場所件数マスタ出力
               PERFORM  JHMKENL1-WRT-SEC
     END-IF.
*
     IF        CNT-MAISU     >    ZERO
               MOVE     321717    TO    HEN-F01
*              当日スケジュールマスタ出力
               PERFORM   JHMTJSL1-WRT-SEC
     END-IF.
*
     IF        CNT-MAISU1    >    ZERO
               MOVE     7         TO    HEN-F01
*              当日スケジュールマスタ出力
               PERFORM   JHMTJSL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  JHSHENL1
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2
               JHMKENL1  JHMTJSL1
               SKENSYUD  SSHOHIND
     IF        SIHAR-OP-FLG   =   1
               CLOSE     SSIHARAD
     END-IF.
*
     OPEN     I-O   SKENSYUD
     READ     SKENSYUD  AT  END
              GO    TO  END-090
     END-READ.
     MOVE     KENSYU-CNT     TO   SKEN-REC034.
     REWRITE  SKENREC.
*
 END-090.
     CLOSE    SKENSYUD.
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     IF        CNT-MAISU1    >    ZERO
               PERFORM   VLD500-OUTPUT1-SEC
     END-IF.
     CLOSE     VLD500.
*
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JHMTJSL1-WRT-SEC        SECTION.
*
     MOVE   "JHMTJSL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     READ    JHMTJSL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JHMTJSL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
*****MOVE    CNT-KENSU     TO        TJS-F08.
*****MOVE    CNT-MAISU     TO        TJS-F09.
     EVALUATE HEN-F01
         WHEN 321717  MOVE CNT-KENSU  TO TJS-F08
                      MOVE CNT-MAISU  TO TJS-F09
         WHEN 7       MOVE CNT-KENSU1 TO TJS-F08
                      MOVE CNT-MAISU1 TO TJS-F09
     END-EVALUATE.
     WRITE   TJS-REC.
     GO      TO   JHMTJSL1-WRT-EXIT.
*
 JHMTJSL1-010.
*
*****MOVE    CNT-KENSU     TO        TJS-F08.
*****MOVE    CNT-MAISU     TO        TJS-F09.
     EVALUATE HEN-F01
         WHEN 321717  MOVE CNT-KENSU  TO TJS-F08
                      MOVE CNT-MAISU  TO TJS-F09
         WHEN 7       MOVE CNT-KENSU1 TO TJS-F08
                      MOVE CNT-MAISU1 TO TJS-F09
     END-EVALUATE.
     REWRITE TJS-REC.
*
 JHMTJSL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
*****MOVE      HEN-F01            TO    VLD-F13.
     MOVE      321717             TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT1-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT1-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      7                  TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT1-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
