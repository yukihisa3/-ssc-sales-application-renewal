# SJH0019L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0019L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票番号重複確認リスト　　　　　　*
*    モジュール名　　　　：　伝票番号重複確認リスト　　　　　　*
*    作成日／更新日　　　：　2001/03/19                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新日／更新者　　　：　2011/10/04 / YOSHIDA.M            *
*    修正概要　　　　　　：　基幹サーバ統合                    *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJH0019L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/03/19.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F01
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
***2011.10.04(DEN-F07,DEN-F112)
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         HTENMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       STATUS          ZSOKMS-ST.
*----<< 伝票番号重複確認Ｆ >>--*
     SELECT  DENKANF   ASSIGN    TO        DENKANL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KAN-F01
                                           KAN-F02
                                           KAN-F03
                                           KAN-F04
                       STATUS              DENKANF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 倉庫マスタ>>-*
 FD  ZSOKMS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< 伝票番号重複確認リスト >>-*
 FD  DENKANF
                        LABEL RECORD   IS   STANDARD.
     COPY     DENKANF   OF        XFDLIB
              JOINING   KAN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01)  VALUE  ZERO.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG        PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)  VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)  VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  ZSOKMS-ST         PIC  X(02).
 01  DENKANF-ST        PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-SYSTIME.
     03  WK-SYSHH       PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-SYSMN       PIC  9(02).
     03  FILLER         PIC  X(01)     VALUE ":".
     03  WK-SYSSS       PIC  9(02).
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH          PIC   9(02)  VALUE  ZERO.
     03  WK-MM          PIC   9(02)  VALUE  ZERO.
*
 01  WK-SOKOCD          PIC  X(02)   VALUE  SPACE.
*開始，終了倉庫コード
 01  SOKCD1             PIC  X(02)   VALUE  ZERO.
 01  SOKCD2             PIC  X(02)   VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-32          PIC  X(25).
     03  WK-33          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-34          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-35          PIC  S9(12)V99   PACKED-DECIMAL.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE
         NC"_取　消　_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_戻る".
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SJH0019L".
         05  FILLER     PIC  X(28)     VALUE  SPACE.
         05  FILLER     PIC  N(17)     VALUE
             NC"【　　伝票番号重複確認リスト　　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(14)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  ZZZ9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"倉庫：".
         05  HD-031     PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-032     PIC  N(18).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"バッチ_：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-041     PIC  99999999.
         05  FILLER     PIC  X(01)     VALUE  "-".
         05  HD-042     PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  "-".
         05  HD-043     PIC  99999999.
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"相".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(22)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"出場".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝場".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文_".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"分類".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(02)     VALUE  NC"商区".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝発".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"請区".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"量販商品".
         05  FILLER     PIC  X(42)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"訂正数量".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
 01  P-LINE1                     CHARACTER  TYPE  MODE-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02).
         05  ME-03      PIC  9(01).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-08      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  N(04).
         05  FILLER     PIC  X(01).
         05  ME-13      PIC  X(07).
         05  FILLER     PIC  X(01).
         05  ME-14      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(04).
         05  FILLER     PIC  X(03).
         05  ME-17      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-19      PIC  9(01).
         05  FILLER     PIC  X(05).
     03  FILLER         CHARACTER TYPE MODE-2.
         05  ME-20      PIC  N(02).
 01  MEIS011.
     03  FILLER.
         05  FILLER     PIC  X(27)  VALUE  SPACE.
         05  FILLER     PIC  X(01)  VALUE  "(".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  ME-36      PIC  9(09).
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  X(01)  VALUE  ")".
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  Z9.
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  X(30).
         05  ME-241      PIC  --,---,--9.99.
         05  FILLER     PIC  X(02).
         05  ME-242     PIC  X(01)     VALUE  "(".
         05  ME-243     PIC  --,---,--9.99.
         05  ME-244     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(02).
         05  ME-25      PIC  X(01).
         05  ME-26      PIC  --,---,--9.99.
         05  ME-27      PIC  ---,---,--9.
         05  ME-28      PIC  --,---,--9.99.
         05  ME-29      PIC  ---,---,--9.
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  ME-32L     PIC  N(03)     VALUE  NC"（備考".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-32      PIC  X(25).
         05  ME-32R     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"合　計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-33      PIC  ---,---,--9.99.
         05  FILLER     PIC  X(32)     VALUE  SPACE.
         05  ME-34      PIC  ----,---,--9.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  ME-35      PIC  ----,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "-".
*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-JDATE             PIC   9(08).
 01  LINK-JTIME             PIC   9(04).
 01  LINK-JTOKCD            PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-JTOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH0019L SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF HTOKMS HTENMS ZSOKMS DENKANF PRTF.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH0019L HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF HTOKMS HTENMS ZSOKMS DENKANF PRTF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH0019L HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF HTOKMS HTENMS ZSOKMS DENKANF PRTF.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH0019L ZSOKMS ERROR " ZSOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF HTOKMS HTENMS ZSOKMS DENKANF PRTF.
     STOP     RUN.
*----<< 伝票番号重複確認Ｆ >>--*
 DENKANF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DENKANF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SJH0019L DENKANF ERROR " DENKANF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF HTOKMS HTENMS ZSOKMS DENKANF PRTF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     SYS-HH         TO   WK-SYSHH.
     MOVE     SYS-MN         TO   WK-SYSMN.
     MOVE     SYS-SS         TO   WK-SYSSS.
*
     DISPLAY  "*** SJH0019L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     DENKANF.
     OPEN     OUTPUT    PRTF.
*----<< 初期ラインカウントセット >>
     MOVE     99          TO  LINE-CNT.
     INITIALIZE               GOKEI-AREA.
*----<< 伝票番号重複確認Ｆスタート >>
     MOVE     SPACE       TO  KAN-REC.
     INITIALIZE               KAN-REC.
     MOVE     LINK-JDATE  TO  KAN-F01.
     MOVE     LINK-JTIME  TO  KAN-F02.
     MOVE     LINK-JTOKCD TO  KAN-F03.
     START    DENKANF  KEY  IS  >=  KAN-F01 KAN-F02
                                    KAN-F03 KAN-F04

              INVALID
              MOVE     "END"    TO   END-FLG
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃" UPON CONS
              DISPLAY NC"＃　　　　　　　　＃" UPON CONS
              DISPLAY NC"＃　伝票重複なし　＃" UPON CONS
              DISPLAY NC"＃　　　　　　　　＃" UPON CONS
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃" UPON CONS
              GO                TO   100-INIT-RTN-EXIT
     END-START.
*
     PERFORM  900-DENKANF-READ.
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃" UPON CONS
              DISPLAY NC"＃　　　　　　　　＃" UPON CONS
              DISPLAY NC"＃　伝票重複なし　＃" UPON CONS
              DISPLAY NC"＃　　　　　　　　＃" UPON CONS
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃" UPON CONS
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    売上伝票Ｆスタート
     MOVE     SPACE               TO   CHK-FLG.
     MOVE     SPACE               TO   DEN-REC.
     INITIALIZE                        DEN-REC.
     MOVE     KAN-F03             TO   DEN-F01.
     MOVE     KAN-F04             TO   DEN-F02.
     MOVE     ZERO                TO   DEN-F04.
     MOVE     "40"                TO   DEN-F051.
     MOVE     ZERO                TO   DEN-F03.
     PERFORM  900-DEN-START-READ.
     IF       CHK-FLG NOT = "END"
              PERFORM  900-DEN-READ
     ELSE
              GO                  TO   MAIN010
     END-IF.
*
     PERFORM  241-LIST-PRINT  UNTIL  CHK-FLG = "END".
*合計欄出力
     PERFORM   2413-MEIS03-PRINT.
*
 MAIN010.
     PERFORM  900-DENKANF-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    SHTDENF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    ZSOKMS.
     CLOSE    DENKANF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SJH0019L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
     MOVE    "241-LIST-PRINT"     TO   S-NAME.
*
*伝票ヘッダ行出力
     IF       DEN-F03   =   1
              PERFORM   2411-MEIS01-PRINT
     END-IF.
*明細行出力
     PERFORM   2412-MEIS02-PRINT.
*伝票合計集計
     PERFORM   2414-SYUKEI.
*売上伝票Ｆ読み込み
     PERFORM   900-DEN-READ.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-PRINT      SECTION.
     MOVE    "2411-MEIS01-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    62
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     DEN-F04        TO   ME-03.
     MOVE     DEN-F01        TO   ME-04.
     MOVE     DEN-F01        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-05.
     MOVE     DEN-F02        TO   ME-06.
     MOVE     KAN-F05        TO   ME-36.
     MOVE     DEN-F08        TO   ME-07.
     MOVE     DEN-F09        TO   ME-08.
     MOVE     DEN-F07        TO   ME-09.
     MOVE     DEN-F01        TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-10.
     MOVE     DEN-F051       TO   ME-11.
     MOVE     DEN-F052       TO   ME-12.
     MOVE     DEN-F10        TO   ME-13.
     MOVE     DEN-F111       TO   ME-14.
     MOVE     DEN-F112       TO   ME-15.
     MOVE     DEN-F12        TO   ME-16.
     MOVE     DEN-F131       TO   ME-17.
     MOVE     DEN-F132       TO   ME-18.
     MOVE     DEN-F134       TO   ME-19.
     IF       DEN-F133  =    9
              MOVE      NC"請求"  TO   ME-20
     ELSE
              MOVE      SPACE     TO   ME-20
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS01    AFTER     2.
     WRITE    PRT-REC        FROM MEIS011   AFTER     1.
     ADD      3              TO   LINE-CNT.
 2411-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-MEIS02-PRINT      SECTION.
     MOVE    "2412-MEIS02-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    63
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS02.
     MOVE     DEN-F03        TO   ME-21.
     IF       DEN-F25   NOT  =    SPACE
              MOVE      DEN-F25   TO   ME-22
     ELSE
              MOVE      DEN-F141  TO   ME-22
     END-IF.
     MOVE     DEN-F142       TO   ME-23.
     MOVE     DEN-F50        TO   ME-241.
     MOVE     DEN-F16        TO   ME-25.
     MOVE     DEN-F512       TO   ME-26.
     MOVE     DEN-F521       TO   ME-27.
     MOVE     DEN-F513       TO   ME-28.
     MOVE     DEN-F522       TO   ME-29.
*
     MOVE     "("            TO   ME-242.
***  訂正時のみ出力
     IF       DEN-F15  NOT = DEN-F50
              MOVE     DEN-F15        TO   ME-243
     END-IF.
     MOVE      ")"           TO   ME-244.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS02    AFTER     1.
     ADD      1              TO   LINE-CNT.
 2412-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2413-MEIS03-PRINT      SECTION.
     MOVE    "2413-MEIS03-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    62
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS03.
     MOVE     WK-33          TO   ME-33.
     MOVE     WK-34          TO   ME-34.
     MOVE     WK-35          TO   ME-35.
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM P-LINE2   AFTER     1.
     ADD      3              TO   LINE-CNT.
     INITIALIZE              GOKEI-AREA.
 2413-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 2414-SYUKEI            SECTION.
     MOVE    "2414-SYUKEI"   TO   S-NAME.
     ADD      DEN-F50        TO   WK-33.
     ADD      DEN-F521       TO   WK-34.
     ADD      DEN-F522       TO   WK-35.
 2414-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2415-HEAD-PRINT        SECTION.
     MOVE    "2415-HEAD-PRINT"    TO   S-NAME.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YYW             TO   HD-011.
     MOVE     SYS-MMW             TO   HD-012.
     MOVE     SYS-DDW             TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*
     MOVE     DEN-F48             TO   HD-031  SOK-F01.
     READ     ZSOKMS
       INVALID
              MOVE      SPACE     TO   HD-032
       NOT INVALID
              MOVE      SOK-F02   TO   HD-032
     END-READ.
     MOVE     DEN-F46             TO   HD-041.
     MOVE     DEN-F47             TO   HD-042.
     MOVE     DEN-F01             TO   HD-043.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
 CHK040.
     MOVE     9              TO   LINE-CNT.
 2415-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     MOVE     "900-DEN-START-READ"     TO   S-NAME.
     START    SHTDENF   KEY  >=   DEN-F01
                                  DEN-F02
                                  DEN-F04
                                  DEN-F051
***2011.10.04(DEN-F07,DEN-F112)
                                  DEN-F07
                                  DEN-F112
                                  DEN-F03
              INVALID   KEY
                        MOVE "END"     TO   CHK-FLG
     END-START.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
*
     READ     SHTDENF   AT   END
              MOVE      "END"     TO   CHK-FLG
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*
     IF       KAN-F03  =  DEN-F01
     AND      KAN-F04  =  DEN-F02
              CONTINUE
     ELSE
              MOVE      "END"     TO   CHK-FLG
              GO   TO   900-DEN-READ-EXIT
     END-IF.
*
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票番号重複確認Ｆ読み込み                   *
*--------------------------------------------------------------*
 900-DENKANF-READ       SECTION.
     MOVE     "900-DENKANF-READ"  TO   S-NAME.
*
     READ     DENKANF   AT   END
              MOVE      "END"     TO   END-FLG
              GO   TO   900-DENKANF-READ-EXIT
     END-READ.
*
     IF       KAN-F01  =  LINK-JDATE
     AND      KAN-F02  =  LINK-JTIME
     AND      KAN-F03  =  LINK-JTOKCD
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
     END-IF.
*
 900-DENKANF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
