# NSY0040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0040B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    業務名　　　　　　　：　発注受信業務　　　　　　　　　　　*
*    モジュール名　　　　：　ＤＣＭ発注受信ＤＴ変換処理　　　　*
*    作成日／更新日　　　：　2021/02/12                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭ基本情報ファイルより、当日　*
*                            売上伝票Ｆを作成する。　　　　　　*
*    取引先名　　　　　　：　ＤＣＭ全取引先共通　　　　        *
**履歴**********************************************************
*    2021/02/12  高橋　　新規作成　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NSY0040B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          17/02/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＤＣＭ基本情報ファイル
     SELECT   DNJOHOF   ASSIGN    TO        DA-01-VI-DNJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DJS-K01   DJS-K02
                                            DJS-K03   DJS-K04
                                            DJS-K05   DJS-K06
                                            DJS-K07   DJS-K08
                        FILE      STATUS    DJS-STATUS.
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
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*発注種別変換マスタ
     SELECT   DCMHSBL2  ASSIGN    TO        DA-01-VI-DCMHSBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F02
                        FILE  STATUS   IS   HSB-STATUS.
*#2019/03/18 NAV ED
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＤＣＭ基本情報ファイル
******************************************************************
 FD  DNJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DNJOHOF   OF        XFDLIB
              JOINING   DJS       PREFIX.
*
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
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     DCMDENF   OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
*
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
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*#2019/03/18 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL2           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/18 NAV ED
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TORICD               PIC  9(08)     VALUE  ZERO.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  FURIWAKE-CD             PIC  9(01)     VALUE  ZERO.
 01  DAIHYO-BASYO-CD         PIC  X(02)     VALUE  SPACE.
 01  JISYA-TORICD            PIC  X(08)     VALUE  SPACE.
 01  SYUKA-BASYO             PIC  X(02)     VALUE  SPACE.
 01  WK-RUTO-CD              PIC  X(03)     VALUE  SPACE.
 01  WK-SYUKA-BASYO          PIC  X(02)     VALUE  SPACE.
 01  DCMHSBL2-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-CENTER               PIC  X(01)     VALUE  SPACE.
 01  WK-RUTO-HENKAN          PIC  X(10)     VALUE  SPACE.
*
*ブレイクキー
 01  WK-BRK-KEY.
     03  BRK-K05           PIC  9(05).
     03  BRK-K06           PIC  9(09).
     03  BRK-K07           PIC  9(02).
     03  BRK-K08           PIC  9(08).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC  9(06).
     03  SYS-DATEW         PIC  9(08).
 01  WK-ST.
     03  DJS-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  HSB-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NSY0040B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0040B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0040B".
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
*    ルート変換
 01  WK-RUTO.
     03  WK-RUTO-S          PIC       X(02).
     03  WK-RUTO-S-R        REDEFINES WK-RUTO-S.
         05  WK-RUTO-H      PIC       9(02).
 01  WK-DEN-NO.
     03  WK-DEN-TEN        PIC 9(03).
     03  WK-DEN-DEN        PIC 9(06).
*
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
     03  PARA-JTOKCD        PIC   9(08).
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
                        PROCEDURE   DNJOHOF.
     MOVE      "DNJOHOL1"   TO   AB-FILE.
     MOVE      DJS-STATUS   TO   AB-STS.
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
                        PROCEDURE   JSMKENL1.
     MOVE      "JSMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
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
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*#2019/03/18 NAV ST
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMHSBL2.
     MOVE      "DCMHSBL2"   TO   AB-FILE.
     MOVE      HSB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*#2019/03/18 NAV ED
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
     OPEN     INPUT     DNJOHOF
                        SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
     OPEN     INPUT     TENMS1.
     OPEN     OUTPUT    VLD500.
     OPEN     INPUT     DCMHSBL2.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     INITIALIZE                   WK-BRK-KEY.
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
*取引先マスタ索引
     MOVE     PARA-JTOKCD       TO   WK-TORICD.
     DISPLAY "## DCM-TORICD = " WK-TORICD  UPON CONS.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  = "INV"
         MOVE  4000          TO   PROGRAM-STATUS
         DISPLAY NC"＃取引先マスタ無　異常！！＃" UPON CONS
         STOP  RUN
     ELSE
*********振分倉庫ＣＤ／代表場所ＣＤ
         MOVE  TOK-F95       TO   FURIWAKE-CD
         MOVE  TOK-F81       TO   DAIHYO-BASYO-CD
         MOVE  TOK-F52       TO   JISYA-TORICD
        DISPLAY "# TOK-F01      = " TOK-F01      UPON CONS
        DISPLAY "# JISYA-TORICD = " JISYA-TORICD UPON CONS
     END-IF.
*ＤＣＭ基本情報Ｆスタート
     MOVE      SPACE         TO   DJS-REC.
     INITIALIZE                   DJS-REC.
     MOVE      PARA-JDATE    TO   DJS-K01.
     MOVE      PARA-JTIME    TO   DJS-K02.
     MOVE      PARA-JTOKCD   TO   DJS-K03.
     START     DNJOHOF  KEY  IS  >=  DJS-K01  DJS-K02  DJS-K03
                                     DJS-K04  DJS-K05  DJS-K06
                                     DJS-K07  DJS-K08
               INVALID
               DISPLAY NC"＃対象ＤＴ存在無１！！＃＃" UPON CONS
               DISPLAY NC"＃" " DJS-K01 = " DJS-K01   UPON CONS
               DISPLAY NC"＃" " DJS-K02 = " DJS-K02   UPON CONS
               DISPLAY NC"＃" " DJS-K03 = " DJS-K03   UPON CONS
               MOVE    4000          TO     PROGRAM-STATUS
               MOVE    9             TO     END-FG
               GO                    TO     INIT-EXIT
     END-START.
*
     PERFORM  DNJOHOF-READ-SEC.
     IF  END-FG  =  9
         DISPLAY NC"＃対象ＤＴ存在無２！！＃＃" UPON CONS
         DISPLAY NC"＃" " DJS-K01 = " DJS-K01   UPON CONS
         DISPLAY NC"＃" " DJS-K02 = " DJS-K02   UPON CONS
         DISPLAY NC"＃" " DJS-K03 = " DJS-K03   UPON CONS
         MOVE    4000          TO     PROGRAM-STATUS
         MOVE    9             TO     END-FG
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DNJOHOF-READ-SEC      SECTION.
*
     READ  DNJOHOF  NEXT  AT  END
           MOVE     9         TO     END-FG
           GO                 TO     DNJOHOF-READ-EXIT
     END-READ.
*バッチ番号判定　　
     IF    PARA-JDATE  =  DJS-K01
     AND   PARA-JTIME  =  DJS-K02
     AND   PARA-JTOKCD =  DJS-K03
           CONTINUE
     ELSE
           MOVE     9         TO     END-FG
           GO                 TO     DNJOHOF-READ-EXIT
     END-IF.
*
     ADD   1                  TO     RD-CNT.
*
     IF  RD-CNT(6:3)  =  "000" OR "500"
         DISPLAY "(" WK-TORICD ")READ-CNT = " RD-CNT UPON CONS
     END-IF.
*
 DNJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
**   DISPLAY "CHK1 = " DJS-K05 " - " BRK-K05 UPON CONS.
**   DISPLAY "CHK2 = " DJS-K06 " - " BRK-K06 UPON CONS.
**   DISPLAY "CHK3 = " DJS-K07 " - " BRK-K07 UPON CONS.
**   DISPLAY "CHK4 = " DJS-K08 " - " BRK-K08 UPON CONS.
     IF    DJS-K05  =   BRK-K05
     AND   DJS-K06  =   BRK-K06
     AND   DJS-K07  =   BRK-K07
     AND   DJS-K08  =   BRK-K08
           CONTINUE
     ELSE
*          出荷場所件数マスタ出力
           IF   CNT-MAISU     >    ZERO
                PERFORM  JSMKENL1-WRT-SEC
           END-IF
           ADD       1        TO   CNT-MAISU
           MOVE      ZERO     TO   CNT-KENSU-D
           MOVE      ZERO     TO   CNT-GYO
           MOVE      DJS-K05  TO   BRK-K05
           MOVE      DJS-K06  TO   BRK-K06
           MOVE      DJS-K07  TO   BRK-K07
           MOVE      DJS-K08  TO   BRK-K08
           MOVE      DJS-K04  TO   WK-RUTO-CD
***********DISPLAY "DJS-K04 = " DJS-K04 UPON CONS
     END-IF.
*明細行
     ADD   1                  TO   CNT-KENSU.
     ADD   1                  TO   CNT-KENSU-D.
     PERFORM  EDIT-SEC
*
     PERFORM  DNJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC.
     INITIALIZE                       HEN-REC.
*各ＤＣＭ固有領域
*    発注伝票番号
     MOVE     DJS-F03       TO        HEN-M01.
*    特売ＣＤ
     MOVE     DJS-F061      TO        HEN-M02.
*    発注種別区分
     MOVE     DJS-F09       TO        HEN-M03.
*    納品区分
     MOVE     DJS-F10       TO        HEN-M04.
*    ゾーンＣＤ
     MOVE     DJS-F17       TO        HEN-M05.
*    ブロックＣＤ
     MOVE     DJS-F201      TO        HEN-M06.
*    納品先ＣＤ　
     MOVE     DJS-F21       TO        HEN-M07.
*    納品先センターブロックＣＤ
     MOVE     DJS-F234      TO        HEN-M08.
*    納品先センターＣＤ
     MOVE     DJS-F24       TO        HEN-M09.
*    館番号
     MOVE     DJS-F264      TO        HEN-M10.
*    共通取引先ＣＤ　　　
     MOVE     DJS-F27       TO        HEN-M11.
*    個別取引先ＣＤ　　　
     MOVE     DJS-F304      TO        HEN-M12.
*    直送ＣＤ（ルート）
     MOVE     DJS-F31       TO        HEN-M13.
*    ゴンドラＮＯ
     MOVE     DJS-M086      TO        HEN-M14.
*    発注単位区分
     MOVE     DJS-M09       TO        HEN-M15.
*    形状区分
     MOVE     DJS-M091      TO        HEN-M16.
*    部門　　
     MOVE     DJS-F14       TO        HEN-M17.
*    発注種別変換区分
*    発注種別変換マスタより取得
     MOVE     DJS-K03                  TO   HSB-F01
     MOVE     DJS-F09                  TO   HSB-F02
     PERFORM  DCMHSBL2-READ-SEC
     IF       DCMHSBL2-INV-FLG  =  SPACE
              MOVE   HSB-F03           TO   HEN-M18
     ELSE
              MOVE   "0"               TO   HEN-M18
     END-IF.
*****転送処理の中でセット
*転送処理へ
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
     MOVE    "TENSO-SEC"   TO        S-NAME.
*取引先コード
     MOVE        DJS-K03   TO        HEN-F01.
*伝票ナンバー
     MOVE        DJS-K06   TO        HEN-F02  HEN-F23.
*行番号
     MOVE        DJS-K07   TO        HEN-F03.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        DJS-K05   TO        HEN-F07.
*発注納品方法変更対応
*店舗マスタ検索（センター区分）
     MOVE        SPACE           TO        TEN-REC.
     INITIALIZE                            TEN-REC.
     MOVE        HEN-F01         TO        TEN-F52.
     MOVE        HEN-F07         TO        TEN-F011.
     READ    TENMS1
       INVALID
         MOVE    SPACE           TO        TEN-REC
         INITIALIZE                        TEN-REC
     END-READ.
*センター区分
     MOVE        SPACE     TO        HEN-F42.
*****↓受信データの納品区分で判断する場合
     IF  HEN-M04  =  "1"  OR  "4"
         IF   DJS-F24  =  SPACE
              MOVE   "1"   TO        WK-CENTER
         ELSE
              MOVE   "2"   TO        WK-CENTER
         END-IF
     ELSE
         MOVE SPACE        TO        WK-CENTER
     END-IF.
*
     IF  WK-CENTER  =  "1"  OR  "2"
         MOVE    1               TO        HEN-F40
*        発注種別変換マスタより取得
         IF  DCMHSBL2-INV-FLG  =  SPACE
                  MOVE   HSB-F05           TO   HEN-F42(1:5)
         ELSE
                  MOVE   "*****"           TO   HEN-F42(1:5)
         END-IF
*
         EVALUATE  HEN-M16
             WHEN  "0"  MOVE "0:ﾅｼ"        TO   HEN-F42(6:5)
             WHEN  "1"  MOVE "1:ｺﾓﾉ"       TO   HEN-F42(6:5)
             WHEN  "2"  MOVE "2:ｲｹｲ"       TO   HEN-F42(6:5)
             WHEN  "3"  MOVE "3:ｹｰｽ"       TO   HEN-F42(6:5)
             WHEN  "4"  MOVE "4:ｶｺﾞ"       TO   HEN-F42(6:5)
             WHEN OTHER MOVE "********"    TO   HEN-F42(6:5)
         END-EVALUATE
         IF  WK-CENTER  =  "2" *>店舗梱包でセンター経由の場合
             MOVE      SPACE              TO   WK-RUTO-HENKAN
             MOVE      "TS"               TO   WK-RUTO-HENKAN(1:2)
             MOVE      HEN-F42(1:5)       TO   WK-RUTO-HENKAN(3:5)
             MOVE      HEN-F42(8:3)       TO   WK-RUTO-HENKAN(8:3)
             MOVE      WK-RUTO-HENKAN     TO   HEN-F42
*************EVALUATE  HEN-M16
*              WHEN  "0"  MOVE "0ﾅｼ T"    TO   HEN-F42(6:5)
*              WHEN  "1"  MOVE "1ｺﾓﾉT"    TO   HEN-F42(6:5)
*              WHEN  "2"  MOVE "2ｲｹｲT"    TO   HEN-F42(6:5)
*              WHEN  "3"  MOVE "3ｹｰｽT"    TO   HEN-F42(6:5)
*              WHEN  "4"  MOVE "4ｶｺﾞT"    TO   HEN-F42(6:5)
*              WHEN OTHER MOVE "********" TO   HEN-F42(6:5)
*************END-EVALUATE
         END-IF
     ELSE
         MOVE    ZERO            TO        HEN-F40
         MOVE " :ﾃﾝﾎﾟﾁｮｸｿｳ"      TO        HEN-F42
     END-IF.
*発注納品方法変更対応
*  商品変換テーブル検索
     MOVE        SPACE     TO        TBL-REC.
     INITIALIZE                      TBL-REC.
     MOVE        HEN-F01   TO        TBL-F01.
     MOVE        DJS-M02   TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*発注日
     MOVE      DJS-F05     TO        HEN-F111.
*発注日
     MOVE      DJS-K08     TO        HEN-F112.
*分類（部門）
     MOVE      DJS-F14     TO        HEN-F12.
*商品区分　　
     MOVE      SPACE       TO        HEN-F131.
*伝票区分　　
     MOVE      DJS-F081    TO        HEN-F132.
*伝発区分
     MOVE      9           TO        HEN-F134.
*自社商品コード
     MOVE      TBL-F031    TO        HEN-F1411.
*自社商品単品コード
     MOVE      TBL-F032    TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE        TBL-F031  TO        MEI-F011.
     MOVE        TBL-F032  TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名（商品名）
         MOVE    DJS-M07(1:15)  TO   HEN-F1421
         MOVE    DJS-M07(16:5)  TO   HEN-F1422(1:5)
*        商品名（規格）
         MOVE    DJS-M084(1:10) TO   HEN-F1422(6:10)
       NOT INVALID
*        商品名（商品名）
         MOVE    DJS-M07(1:15)  TO   HEN-F1421
         MOVE    DJS-M07(16:5)  TO   HEN-F1422(1:5)
*        商品名（規格）
         MOVE    DJS-M084(1:10) TO   HEN-F1422(6:10)
     END-READ.
*数量
     COMPUTE  HEN-F15  =  DJS-M11  /  100.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     COMPUTE  HEN-F172 =  DJS-M16  /  100.
*売価単価
     MOVE      DJS-M17     TO        HEN-F173.
*原価金額
     COMPUTE  HEN-F181  =  HEN-F15  *  HEN-F172.
*売価金額
     COMPUTE  HEN-F182  =  HEN-F15  *  HEN-F173.
*店舗名（備考）
*****MOVE      HEN-A24     TO        HEN-F22.
*自社得意先コード
     MOVE   JISYA-TORICD   TO        HEN-F24.
*相手商品コード
     MOVE      DJS-M02     TO        HEN-F25.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE        1         TO        HEN-F275.
*付番区分
     MOVE        9         TO        HEN-F276.
*量販店区分
     MOVE       "A"        TO        HEN-F278.
*ＷＳ番号
     MOVE        1         TO        HEN-F28.
*変換値
*****MOVE        WK-NEN    TO        HEN-F29.
*店舗名（カナ）
     MOVE      DJS-F22     TO        HEN-F30.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
     MOVE     WK-RUTO-CD   TO        HEN-F48.
*振分倉庫ＣＤを退避する
     MOVE  HEN-F48         TO        WK-SYUKA-BASYO.
*_番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        HEN-F15   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-F172  TO        HEN-F512.
*修正売価単価
     MOVE        HEN-F173  TO        HEN-F513.
*修正原価金額
     MOVE        HEN-F181  TO        HEN-F521.
*修正売価金額
     MOVE        HEN-F182  TO        HEN-F522.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     MOVE    WK-SYUKA-BASYO TO       KEN-F04.
*
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     MOVE    WK-SYUKA-BASYO TO       KEN-F04.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D   TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU     >    ZERO
*              出荷場所件数マスタ出力
               PERFORM  JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     DNJOHOF  JHSHENL1
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2
               JSMKENL1  JSMDAYL1.
     CLOSE     TENMS1.
     CLOSE     DCMHSBL2.
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     CLOSE     VLD500.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
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
     MOVE      HEN-F01            TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　取引先マスタ索引
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE    SPACE         TO        TOK-REC.
     INITIALIZE                      TOK-REC.
     MOVE    WK-TORICD     TO        TOK-F01.
     READ    TOKMS2
             INVALID
             MOVE  "INV"   TO        HTOKMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO        HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL2-READ-SEC         SECTION.
*
     READ     DCMHSBL2
         INVALID
           MOVE  "INV"     TO        DCMHSBL2-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL2-INV-FLG
     END-READ.
*
 DCMHSBL2-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
