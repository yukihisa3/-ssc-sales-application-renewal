# NSY0210L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0210L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　オンライン　　　*
*    モジュール名　　　　：　出荷リスト発行　　　　　　　　　　*
*    作成日　　　　　　　：　2021/02/18                        *
*    作成者　　　　　　　：　NAV                               *
*    処理概要　　　　　　：　ＤＣＭ出荷情報データを読み、出荷　*
*                            リストを発行する。　　　　　　　　*
**履歴**********************************************************
*    更新日　　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新者　　　　　　　：　　　　　　　　　　　　　　　　　　*
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NSY0210L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/02/18.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   PITCH-2
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YB-22     IS   BAIKAKU-2-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＤＣＭ出荷確定データ >>--*
     SELECT   DNSYUKF   ASSIGN         DA-01-VI-DNSYUKLD
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SYU-K01
                                       SYU-K02
                                       SYU-K03
                                       SYU-K04
                                       SYU-F09
                                       SYU-F05
                                       SYU-K08
                                       SYU-F21
                                       SYU-F264
                                       SYU-F14
*                                      SYU-M091
*                                      SYU-M09
                                       SYU-F03
                                       SYU-M03
                        STATUS         DNSYUKF-ST.
*----<< 発注種別変換マスタ >>--*
     SELECT   DCMHSBF   ASSIGN         DA-01-VI-DCMHSBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HSB-F01
                                       HSB-F02
                        STATUS         DCMHSBF-ST.
*----<<条件ファイル >>--*
     SELECT  HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    HJYOKEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ＤＣＭ出荷確定データ >>--*
 FD  DNSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     DNSYUKF   OF        XFDLIB
              JOINING   SYU       PREFIX.
*----<< 発注種別変換マスタ >>--*
 FD  DCMHSBF            LABEL     RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL     RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT2      PIC  9(03)   VALUE  ZERO.
     03  READ-CNT       PIC  9(08)   VALUE  ZERO.
 01  FLGS.
     03  END-FLG        PIC  X(03)   VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DNSYUKF-ST        PIC  X(02).
 01  DCMHSBF-ST        PIC  X(02).
 01  HJYOKEN-ST        PIC  X(02).
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
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*伝票番号変換
 01  WK-DENPYO          PIC  X(09).
 01  FILLER             REDEFINES      WK-DENPYO.
     03  WK-DENPYO-R    PIC  9(09).
*取引先ＣＤ変換
 01  WK-SYU-F304        PIC  X(06).
 01  FILLER             REDEFINES      WK-SYU-F304.
     03  WK-SYU-F304-H  PIC  9(06).
*
 01  WK-SURYO           PIC  9(05)V9   VALUE  ZERO.
 01  WK-SYUKA           PIC  9(05)V9   VALUE  ZERO.
 01  CNT-AFTER          PIC  9(02)     VALUE  ZERO.
 01  CHK-FLG            PIC  X(03)     VALUE  SPACE.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(09)V99.
     03  WK-GK-BAIKA    PIC  9(09).
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY      PIC  9(04).
     03  WK-H-KU1       PIC  X(01).
     03  WK-H-MM        PIC  9(02).
     03  WK-H-KU2       PIC  X(01).
     03  WK-H-DD        PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-SYU-K01     PIC  9(08).
     03  WK-SYU-K02     PIC  9(04).
     03  WK-SYU-K03     PIC  9(08).
     03  WK-SYU-K04     PIC  X(02).
     03  WK-SYU-F09     PIC  9(02).
     03  WK-SYU-F05     PIC  9(08).
     03  WK-SYU-K08     PIC  9(08).
     03  WK-SYU-F21     PIC  9(04).
     03  WK-SYU-F264    PIC  X(02).
     03  WK-SYU-F14     PIC  9(03).
*    03  WK-SYU-M091    PIC  X(01).
*    03  WK-SYU-M09     PIC  X(01).
     03  WK-SYU-F03     PIC  9(09).
     03  WK-SYU-F03-1   PIC  9(09).
     03  WK-SYU-M03     PIC  9(03).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  TL00.
     05  FILLER         PIC  X(01)     VALUE  SPACE.
     05  TL00-PAGE      PIC  ZZ9.
     05  TL00-KUGIR     PIC  X(01).
     05  TL00-SPAGE     PIC  ZZZ.
     05  FILLER         PIC  X(05)     VALUE  "ﾍﾟｰｼﾞ".
     05  FILLER         PIC  X(105)    VALUE  SPACE.
     05  TL00-DATE      PIC  X(10).
     05  FILLER         PIC  X(02)     VALUE  SPACE.
     05  TL00-TIME      PIC  X(05).
 01  HD001.
     03  FILLER         PIC  X(102)    VALUE  SPACE.
     03  FILLER         PIC  X(33)     VALUE
        "---------------------------------".
 01  HD002.
     03  FILLER         PIC  X(102)    VALUE  SPACE.
     03  FILLER         PIC  X(33)     VALUE
        "!               !               !".
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD01-01    PIC  N(10).
         05  FILLER     PIC  X(32)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-2-5.
         05  FILLER     PIC  N(05)     VALUE
             NC"出荷リスト".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(34)     VALUE  SPACE.
         05  FILLER     PIC  X(04)     VALUE  "!   ".
         05  FILLER     PIC  N(06)     VALUE
             NC"検品／検収者".
         05  FILLER     PIC  X(09)     VALUE  "   !     ".
         05  FILLER     PIC  N(04)     VALUE
             NC"検収印　".
         05  FILLER     PIC  X(05)     VALUE  "    !".
 01  HD011.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  HD011-1    PIC  X(02).
         05  HD011-2    PIC  N(10).
 01  HD012.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(40)     VALUE  SPACE.
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD012-1    PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD012-2    PIC  N(10).
         05  FILLER     PIC  X(01)     VALUE  ")".
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"取引先名称".
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品先".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品先名".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"館名".
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"個別納品先".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(21)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷日".
 01  HD03.
     03  FILLER          CHARACTER TYPE PITCH-1-5.
         05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  HD03-TOKCD  PIC  999999.
         05  FILLER      PIC  X(03)     VALUE  SPACE.
*        05  HD03-JISYA  PIC  X(20).
         05  HD03-JISYA  PIC  N(16).
         05  FILLER      PIC  X(01)     VALUE  SPACE.
*        05  HD03-TENCD  PIC  9999.
         05  HD03-TENCD  PIC  X(07).
         05  FILLER      PIC  X(03)     VALUE  SPACE.
*        05  HD03-TENNM  PIC  X(20).
         05  HD03-TENNM  PIC  N(10).
         05  FILLER      PIC  X(02)     VALUE  SPACE.
*        05  HD03-KANNM  PIC  X(20).
         05  HD03-KANNM  PIC  N(10).
         05  FILLER      PIC  X(04)     VALUE  SPACE.
*        05  HD03-HBUM   PIC  999.
*        05  FILLER      PIC  X(02)     VALUE  SPACE.
*        05  HD03-KEIJY  PIC  N(03).
*        05  FILLER      PIC  X(04)     VALUE  SPACE.
*        05  HD03-HACTN  PIC  N(03).
*        05  FILLER      PIC  X(09)     VALUE  SPACE.
         05  HD03-KBTOKC PIC  X(09).
         05  FILLER      PIC  X(20)     VALUE  SPACE.
         05  HD03-HACD   PIC  X(10).
         05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  HD03-NOUD   PIC  X(10).
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"納品番号".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
         05  FILLER     PIC  X(93)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(06)     VALUE  NC"発注伝票番号".
 01  HD05.
     03  FILLER         CHARACTER TYPE PITCH-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
*        05  FILLER     PIC  X(02)     VALUE  SPACE.
*        05  FILLER     PIC  N(02)     VALUE  NC"商品".
*        05  FILLER     PIC  X(04)     VALUE  "ｺｰﾄﾞ".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  X(07)     VALUE  "JANｺｰﾄﾞ".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"品名".
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"規格".
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"特売".
         05  FILLER     PIC  X(02)     VALUE  "CD".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"申込票_".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"発注数".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"出荷数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"引合".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"売価".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(04)     VALUE  NC"　行番号".
 01  SEN                         CHARACTER  TYPE  MODE-2.
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
*
 01  MS01.
     03  FILLER                  CHARACTER  TYPE  MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO PIC  999999999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-BUMON PIC  XXX.
         05  FILLER     PIC  X(11)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備考：".
         05  FILLER     PIC  X(96)     VALUE  SPACE.
         05  MS01-DENNO2 PIC 999999999.
 01  MS02.
     03  FILLER                  CHARACTER  TYPE  PITCH-1-5.
         05  FILLER      PIC  X(01)     VALUE  SPACE.
         05  MS02-GYO    PIC  999.
         05  FILLER      PIC  X(08)     VALUE  SPACE.
*        05  MS02-SYOCD  PIC  X(08).
*        05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  MS02-JANCD  PIC  X(13).
         05  FILLER      PIC  X(01)     VALUE  SPACE.
*        05  MS02-SYONM1 PIC  X(20).
         05  MS02-SYONM1 PIC  N(10).
         05  FILLER      PIC  X(02)     VALUE  SPACE.
*        05  MS02-SYONM2 PIC  X(20).
         05  MS02-SYONM2 PIC  N(10).
         05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  MS02-TOK    PIC  X(09).
         05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  MS02-MOS    PIC  X(09).
         05  FILLER      PIC  X(04)     VALUE  SPACE.
         05  MS02-SURYO  PIC  Z,ZZZ,ZZ9.
         05  FILLER      PIC  X(01)     VALUE  SPACE.
         05  MS02-SYUKA  PIC  Z,ZZZ,ZZ9.
         05  FILLER      PIC  X(08)     VALUE  " (   )  ".
         05  MS02-BAITAN PIC  Z,ZZZ,ZZ9.
         05  FILLER      PIC  X(01)     VALUE  SPACE.
         05  MS02-BAIKIN PIC  Z,ZZZ,ZZ9.
         05  FILLER      PIC  X(02)     VALUE  SPACE.
         05  MS02-GYO2   PIC  999.
 01  MS03.
     03  FILLER.
         05  FILLER     PIC  X(120)    VALUE  SPACE.
         05  MS03-BAIGOK PIC  ZZ,ZZZ,ZZ9.
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
 LINKAGE     SECTION.
 01  LINK-JDATE             PIC  9(08).
 01  LINK-JTIME             PIC  9(04).
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
 01  LINK-SNOUDT            PIC  9(08).
 01  LINK-ENOUDT            PIC  9(08).
 01  LINK-SDENCD            PIC  9(09).
 01  LINK-EDENCD            PIC  9(09).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT
                                         LINK-SDENCD
                                         LINK-EDENCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ＤＣＭ出荷確定データ >>--*
 DNSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DNSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0210L DNSYUKF ERROR " DNSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000                     TO   PROGRAM-STATUS.
*    CLOSE    DNSYUKF.
     STOP     RUN.
*----<< 発注種別変換マスタ >>--*
 DCMHSBF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DCMHSBF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0210L DCMHSBF ERROR " DCMHSBF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000                     TO   PROGRAM-STATUS.
*    CLOSE    DNSYUKF.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### NSY0210L HJYOKEN ERROR " HJYOKEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000                     TO   PROGRAM-STATUS.
*    CLOSE    DNSYUKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-TIME       FROM TIME.
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
     DISPLAY  "*** NSY0210L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DNSYUKF  DCMHSBF  HJYOKEN.
     OPEN     OUTPUT    PRTF.
*
     INITIALIZE  BREAK-KEY.
*ＤＣＭ出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   SYU-REC.
     INITIALIZE                        SYU-REC.
     MOVE     LINK-JDATE          TO   SYU-K01.
     MOVE     LINK-JTIME          TO   SYU-K02.
     MOVE     LINK-TORICD         TO   SYU-K03.
     MOVE     LINK-SOKO           TO   SYU-K04.
*??? MOVE     LINK-STENCD         TO   SYU-K05.
     DISPLAY  "LINK-JDATE = " LINK-JDATE UPON CONS.
     DISPLAY  "LINK-JTIME = " LINK-JTIME UPON CONS.
     DISPLAY  "LINK-TORICD= " LINK-TORICD UPON CONS.
     DISPLAY  "LINK-SOKO  = " LINK-SOKO  UPON CONS.
     DISPLAY  "LINK-STENCD= " LINK-STENCD UPON CONS.
     DISPLAY  "LINK-SDENCD= " LINK-SDENCD UPON CONS.
     PERFORM  900-SYU-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     SYU-K01    TO   WK-SYU-K01
              MOVE     SYU-K02    TO   WK-SYU-K02
              MOVE     SYU-K03    TO   WK-SYU-K03
              MOVE     SYU-K04    TO   WK-SYU-K04
              MOVE     SYU-F09    TO   WK-SYU-F09
              MOVE     SYU-F05    TO   WK-SYU-F05
              MOVE     SYU-K08    TO   WK-SYU-K08
              MOVE     SYU-F21    TO   WK-SYU-F21
              MOVE     SYU-F264   TO   WK-SYU-F264
*             MOVE     SYU-M091   TO   WK-SYU-M091
*             MOVE     SYU-M09    TO   WK-SYU-M09
              MOVE     SYU-F14    TO   WK-SYU-F14
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT  PAGE-CNT2
              MOVE     SPACE      TO   CHK-FLG
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    発注伝票区分／発注日／出荷日／店ＣＤ／館ＣＤブレイク時
*    改ページ
     IF       SYU-F09   NOT =  WK-SYU-F09
     OR       SYU-F05   NOT =  WK-SYU-F05
     OR       SYU-K08   NOT =  WK-SYU-K08
     OR       SYU-F21   NOT =  WK-SYU-F21
     OR       SYU-F264  NOT =  WK-SYU-F264
*    OR       SYU-M091  NOT =  WK-SYU-M091
*    OR       SYU-M09   NOT =  WK-SYU-M09
     OR       SYU-F14   NOT =  WK-SYU-F14
              PERFORM   GOKEI-WT-SEC
              MOVE      "CHK"     TO  CHK-FLG
              PERFORM   HEAD-WT-SEC
              PERFORM   MEISAI-HEAD-SEC
              MOVE      SYU-F09   TO  WK-SYU-F09
              MOVE      SYU-F05   TO  WK-SYU-F05
              MOVE      SYU-K08   TO  WK-SYU-K08
              MOVE      SYU-F21   TO  WK-SYU-F21
              MOVE      SYU-F264  TO  WK-SYU-F264
*             IF  SYU-F304 = 880 OR 1427
*                 MOVE      SYU-F03(1:7)   TO  WK-SYU-F03
*             ELSE
                  MOVE      SYU-F03        TO  WK-SYU-F03
*             END-IF
*             MOVE      SYU-M091  TO  WK-SYU-M091
*             MOVE      SYU-M09   TO  WK-SYU-M09
              MOVE      SYU-F14   TO  WK-SYU-F14
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
              MOVE      SPACE     TO  CHK-FLG
     END-IF.
*    伝票番号がブレイク
*****MOVE     SYU-F03(1:7)        TO  WK-SYU-F03-1.
*    IF  SYU-F304 = 880 OR 1427
*        MOVE      SYU-F03(1:7)   TO  WK-SYU-F03-1
*    ELSE
         MOVE      SYU-F03        TO  WK-SYU-F03-1
*    END-IF.
     IF       WK-SYU-F03-1   NOT =  WK-SYU-F03
              IF  PAGE-CNT > ZERO
                  PERFORM  GOKEI-WT-SEC
              END-IF
              PERFORM   MEISAI-HEAD-SEC
**************MOVE      SYU-F03   TO  WK-SYU-F03
**************MOVE      SYU-F03(1:7)   TO  WK-SYU-F03
*             IF  SYU-F304 = 880 OR 1427
*                 MOVE      SYU-F03(1:7)   TO  WK-SYU-F03
*             ELSE
                  MOVE      SYU-F03        TO  WK-SYU-F03
*             END-IF
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細行セット
     PERFORM  MEISAI-BODY-SEC.
*    ＤＣＭ出荷確定データ読込み
     PERFORM  900-SYU-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     DISPLAY "ﾀｲｼｮｳDT CNT = " READ-CNT  UPON CONS.
     DISPLAY NC"＃総出力枚数⇒" " " PAGE-CNT NC"枚" UPON CONS.
*
     IF  READ-CNT  >  ZERO
         PERFORM  GOKEI-WT-SEC
         PERFORM  TAIL-WT-SEC
     END-IF.
*
     CLOSE    DNSYUKF  DCMHSBF HJYOKEN PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NSY0210L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              PERFORM  TAIL-WT-SEC
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
              IF  CHK-FLG = "CHK"
                  MOVE      ZERO      TO   PAGE-CNT2
              END-IF
     END-IF.
*    行カウンター初期化
     MOVE     ZERO      TO        LINE-CNT.
*    頁カウンター
     ADD      1         TO        PAGE-CNT  PAGE-CNT2.
*会社名出力
     MOVE     SPACE          TO   HD01-01.
     MOVE     SYU-F132       TO   HD01-01.
*    MOVE     SYU-F304       TO   WK-SYU-F304.
*    IF   WK-SYU-F304-H = 13938 OR 17137
*         MOVE NC"ＤＣＭＤＣＭ株式会社"     TO HD01-01
*    END-IF.
*    IF   WK-SYU-F304-H = 100403 OR 100441 OR 100427
*         MOVE NC"ＤＣＭダイキ株式会社"     TO HD01-01
*    END-IF.
*    IF   WK-SYU-F304-H = 880 OR 882 OR 883 OR 1427 OR 14272 OR 14
*         MOVE NC"ホーマック株式会社　"     TO HD01-01
*    END-IF.
*    IF   WK-SYU-F304-H = 1731 OR 1732 OR 7601 OR 7602
*    OR   173  OR  760
*         MOVE NC"株式会社ケーヨー　　"     TO HD01-01
*    END-IF.
*発注種別区分
     MOVE     SYU-K03        TO   HSB-F01.
     MOVE     SYU-F09        TO   HD012-1
                                  HSB-F02.
     READ     DCMHSBF
              INVALID      MOVE  ALL NC"＊" TO HD012-2
              NOT INVALID  MOVE  HSB-F04    TO HD012-2
     END-READ.
*    EVALUATE SYU-F09
*        WHEN 0
*             MOVE NC"　定　番（定　期）　" TO HD012-2
*        WHEN 1
*             MOVE NC"　特　売　　　　　　" TO HD012-2
*        WHEN 2
*             MOVE NC"　新店改装　　　　　" TO HD012-2
*        WHEN 3
*             MOVE NC"　本部発注（投入）　" TO HD012-2
*        WHEN 5
*             MOVE NC"　特注客注ダイレクト" TO HD012-2
*        WHEN 51
*             MOVE NC"　本部在庫補充　　　" TO HD012-2
*        WHEN 52
*             MOVE NC"　商管在庫補充　　　" TO HD012-2
*        WHEN 71
*             MOVE NC"　用度品　　　　　　" TO HD012-2
*        WHEN 81
*             MOVE NC"　プロモーション　　" TO HD012-2
*        WHEN 82
*             MOVE NC"　改　廃　　　　　　" TO HD012-2
*        WHEN 83
*             MOVE NC"　ＢＹ改廃　　　　　" TO HD012-2
*    END-EVALUATE.
*ブロックＣＤ・名
     MOVE     SYU-F201       TO   HD011-1.
     MOVE     SYU-F2032      TO   HD011-2.
*取引先ＣＤ
     MOVE     SYU-F27        TO   HD03-TOKCD.
*取引先名称
     MOVE     SYU-F302       TO   HD03-JISYA.
*納品先ＣＤ
     MOVE     SYU-F201       TO   HD03-TENCD(1:2).
     MOVE     "-"            TO   HD03-TENCD(3:1).
     MOVE     SYU-F21        TO   HD03-TENCD(4:4).
*    納品先名称
     MOVE     SYU-F232       TO   HD03-TENNM.
*    館名称
     EVALUATE SYU-F264
         WHEN "01"
              MOVE NC"ホームセンター　　　"  TO  HD03-KANNM
         WHEN "02"
              MOVE NC"資材センター　　　　"  TO  HD03-KANNM
         WHEN "03"
              MOVE NC"ホームインテリアセン"  TO  HD03-KANNM
         WHEN "04"
              MOVE NC"ガーデンセンター　　"  TO  HD03-KANNM
         WHEN "05"
              MOVE NC"ペット＆ガーデン　　"  TO  HD03-KANNM
         WHEN "06"
              MOVE NC"資材＆ガーデン　　　"  TO  HD03-KANNM
         WHEN "07"
              MOVE NC"ペット　　　　　　　"  TO  HD03-KANNM
         WHEN "08"
              MOVE NC"２Ｆ　　　　　　　　"  TO  HD03-KANNM
         WHEN "09"
              MOVE NC"サイクルレジャー館　"  TO  HD03-KANNM
         WHEN OTHER
              MOVE NC"＊＊＊＊＊＊＊＊＊＊"  TO  HD03-KANNM
     END-EVALUATE.
*個別取引先ＣＤ
     MOVE     SYU-F201       TO   HD03-KBTOKC(1:2).
     MOVE     "-"            TO   HD03-KBTOKC(3:1).
     MOVE     SYU-F304       TO   HD03-KBTOKC(4:6).
*    部門
*    MOVE     SYU-F14               TO  HD03-HBUM.
*    形状区分
*    EVALUATE SYU-M091
*        WHEN "1"
*             MOVE NC"小物　"       TO  HD03-KEIJY
*        WHEN "2"
*             MOVE NC"異形　"       TO  HD03-KEIJY
*        WHEN "3"
*             MOVE NC"ケース"       TO  HD03-KEIJY
*        WHEN "4"
*             MOVE NC"大カゴ"       TO  HD03-KEIJY
*        WHEN OTHER
*             MOVE NC"＊＊＊"       TO  HD03-KEIJY
*    END-EVALUATE.
*    発注単位区分名
*    EVALUATE SYU-M09
*        WHEN "P"
*             MOVE NC"ピース"       TO  HD03-HACTN
*        WHEN "C"
*             MOVE NC"ケース"       TO  HD03-HACTN
*        WHEN OTHER
*             MOVE NC"＊＊＊"       TO  HD03-HACTN
*    END-EVALUATE.
*    発注日
     MOVE     SYU-F05(1:4)   TO   WK-H-YYYY.
     MOVE     SYU-F05(5:2)   TO   WK-H-MM.
     MOVE     SYU-F05(7:2)   TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-HACD.
*    出荷日
     MOVE     SYU-F0611(1:4) TO   WK-H-YYYY.
     MOVE     SYU-F0611(5:2) TO   WK-H-MM.
     MOVE     SYU-F0611(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-HIZUKE-HENSYU TO HD03-NOUD.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
*    WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD011 AFTER  1.
     WRITE    PRT-REC  FROM  HD001 AFTER  0.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD002 AFTER  1.
     WRITE    PRT-REC  FROM  HD012 AFTER  0.
     WRITE    PRT-REC  FROM  HD001 AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  1.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  2.
     WRITE    PRT-REC  FROM  HD05  AFTER  1.
     WRITE    PRT-REC  FROM  SEN1  AFTER  1.
*行カウント
     MOVE     13             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ヘッダー印字
*--------------------------------------------------------------*
 MEISAI-HEAD-SEC       SECTION.
     MOVE     "MEISAI-HEAD-SEC"    TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  52
            PERFORM   HEAD-WT-SEC
     END-IF.
*    納品番号（伝票番号）
     MOVE      SYU-F03     TO   MS01-DENNO.
*****MOVE      SYU-K06     TO   MS01-DENNO.
*****MOVE      SYU-F03     TO   WK-DENPYO.
*    IF  SYU-F304  =  880  OR  1427
*        MOVE  SYU-F03(1:7) TO  WK-DENPYO(3:7)
*        MOVE  "00"         TO  WK-DENPYO(1:2)
*    ELSE
*        MOVE  SYU-F03      TO  WK-DENPYO
*    END-IF.
*    MOVE      WK-DENPYO-R TO   MS01-DENNO.
*    部門
     MOVE      SYU-F14     TO   MS01-BUMON.
*    発注伝票番号
     MOVE      SYU-F03     TO   MS01-DENNO2.
*****MOVE      SYU-K06     TO   MS01-DENNO2.
*    MOVE      SYU-F03     TO   WK-DENPYO.
*    IF  SYU-F304  =  880  OR  1427
*        MOVE  SYU-F03(1:7) TO  WK-DENPYO(3:7)
*        MOVE  "00"         TO  WK-DENPYO(1:2)
*    ELSE
*        MOVE  SYU-F03      TO  WK-DENPYO
*    END-IF.
*    MOVE      WK-DENPYO-R TO   MS01-DENNO2.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  1.
*
     ADD       1           TO   LINE-CNT.
*
 MEISAI-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     IF     LINE-CNT  >  52
            PERFORM   HEAD-WT-SEC
     END-IF.
*    行
     MOVE   SYU-M03               TO   MS02-GYO.
*    商品ＣＤ
*    MOVE   SYU-M04               TO   MS02-SYOCD.
*    ＪＡＮＣＤ
     MOVE   SYU-M02               TO   MS02-JANCD.
*    品名
*    MOVE   SYU-M07(1:20)         TO   MS02-SYONM1.
     MOVE   SYU-M082              TO   MS02-SYONM1.
*    規格
*****MOVE   SYU-M07(21:15)        TO   MS02-SYONM2.
*    MOVE   SYU-M084(1:20)        TO   MS02-SYONM2.
     MOVE   SYU-M0852             TO   MS02-SYONM2.
*    特売ＣＤ
     MOVE   SYU-F061              TO   MS02-TOK.
*    申込票
     MOVE   SYU-F311              TO   MS02-MOS.
*    発注数
     IF     SYU-M11  >  ZERO
            COMPUTE  MS02-SURYO  =  SYU-M11  /  100
     ELSE
            MOVE     ZERO         TO   MS02-SURYO
     END-IF.
*    出荷数
     MOVE   SYU-A36               TO   MS02-SYUKA WK-SURYO.
*    引合
*    売価
     MOVE   SYU-M17               TO   MS02-BAITAN.
*    売価金額
     COMPUTE  MS02-BAIKIN  =  ( WK-SURYO * SYU-M17  ).
     COMPUTE  WK-GK-BAIKA  =  WK-GK-BAIKA +
                              ( WK-SURYO * SYU-M17  ).
*    発注伝票行番号
     MOVE     SYU-M03         TO    MS02-GYO2.
*    〇以上の場合に出力対象
     WRITE     PRT-REC   FROM  MS02  AFTER  1.
     ADD       1           TO   LINE-CNT.
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細合計印字
*--------------------------------------------------------------*
 GOKEI-WT-SEC                 SECTION.
     MOVE     "GOKEI-WT-SEC" TO   S-NAME.
*    売価金額合計
     MOVE     WK-GK-BAIKA       TO    MS03-BAIGOK.
*
     WRITE     PRT-REC   FROM  MS03  AFTER  1.
     WRITE     PRT-REC   FROM  SEN1  AFTER  1.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
     ADD       2           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-SYU-START-READ     SECTION.
     MOVE     "900-SYU-START-READ"     TO   S-NAME.
     START    DNSYUKF   KEY  >=   SYU-K01  SYU-K02
                                  SYU-K03  SYU-K04
                                  SYU-F09  SYU-F05
                                  SYU-K08  SYU-F21
                                  SYU-F264 SYU-F14
                                  SYU-F03  SYU-M03
              INVALID   KEY
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-SYU-START-READ-EXIT
     END-START.
*
     PERFORM   900-SYU-READ.
*
 900-SYU-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＤＣＭ出荷確定ファイル　 READ                *
*--------------------------------------------------------------*
 900-SYU-READ           SECTION.
     MOVE     "900-SYU-READ"      TO   S-NAME.
*
     READ     DNSYUKF   AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-SYU-READ-EXIT
     END-READ.
*
          IF       SYU-K01    =  LINK-JDATE       AND
                   SYU-K02    =  LINK-JTIME       AND
                   SYU-K03    =  LINK-TORICD
                   CONTINUE
          ELSE
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-SYU-READ-EXIT
          END-IF.
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
*****     IF       SYU-K01   NOT =  LINK-JDATE       OR
*                  SYU-K02   NOT =  LINK-JTIME       OR
*                  SYU-K03   NOT =  LINK-TORICD
*                  MOVE     "END"      TO   END-FLG
*                  GO        TO        900-SYU-READ-EXIT
*****     END-IF
          CONTINUE
     ELSE
****      IF       SYU-K01   NOT =  LINK-JDATE       OR
**                 SYU-K02   NOT =  LINK-JTIME       OR
**                 SYU-K03   NOT =  LINK-TORICD      OR
*                  SYU-K04   NOT =  LINK-SOKO        OR
*******************SYU-F21   NOT =  LINK-STENCD
*                  SYU-K05   NOT =  LINK-ETENCD
*                  MOVE     "END"      TO   END-FLG
***                GO        TO        900-SYU-READ-EXIT
          IF       SYU-K04   =  LINK-SOKO
                   CONTINUE
          ELSE
                   GO        TO        900-SYU-READ
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  SYU-K05
     AND      LINK-ETENCD  >=  SYU-K05
*    IF       LINK-STENCD  <=  SYU-F21
*    AND      LINK-ETENCD  >=  SYU-F21
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-SYU-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  SYU-K08
     AND      LINK-ENOUDT  >=  SYU-K08
              CONTINUE
     ELSE
**************DISPLAY "FFF" UPON CONS
              GO        TO        900-SYU-READ
     END-IF.
*
*伝票番号範囲チェック
*    IF       LINK-SDENCD  <=  SYU-K06
*    AND      LINK-EDENCD  >=  SYU-K06
     IF       LINK-SDENCD  <=  SYU-F03
     AND      LINK-EDENCD  >=  SYU-F03
              CONTINUE
     ELSE
              GO        TO        900-SYU-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-SYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    テイル出力処理　　　　　 READ                *
*--------------------------------------------------------------*
 TAIL-WT-SEC            SECTION.
     MOVE     "TAIL-WT-SEC"       TO   S-NAME.
*改行数算出
     COMPUTE  CNT-AFTER  =  54  -  LINE-CNT.
*テイル行編集
*ページ数セット
     MOVE     PAGE-CNT2     TO   TL00-PAGE.
*総ページ数セット
     MOVE     ZERO          TO   TL00-SPAGE.
     MOVE     SPACE         TO   TL00-KUGIR.
*システム日付
     MOVE     SYS-YYW       TO   TL00-DATE(1:4).
     MOVE     "/"           TO   TL00-DATE(5:1).
     MOVE     SYS-MMW       TO   TL00-DATE(6:2).
     MOVE     "/"           TO   TL00-DATE(8:1).
     MOVE     SYS-DDW       TO   TL00-DATE(9:2).
*システム時刻
     MOVE     SYS-HH        TO   TL00-TIME(1:2).
     MOVE     ":"           TO   TL00-TIME(3:1).
     MOVE     SYS-MN        TO   TL00-TIME(4:2).
*
     IF  CNT-AFTER > 1
         WRITE     PRT-REC   FROM  SEN1  AFTER  CNT-AFTER
     END-IF.
     WRITE     PRT-REC   FROM  TL00  AFTER  1.
*
 TAIL-WT-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
