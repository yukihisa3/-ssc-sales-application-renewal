# NVD0130L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0130L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　
*    モジュール名　　　　：　取込件数リスト　　　　　　　　　
*    作成日／作成者　　　：　2019/12/25    ASS.TAKAHASHI
*    流用元　　　　　　　：　SJR0210L
*    処理概要　　　　　　：　パラメタに合致するレコードより
*    　　　　　　　　　　　　取込件数をリスト出力する。
*    更新履歴　　　　　　：
*    　　　　　　　　　　　
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               NVD0130L.
 AUTHOR.                   NAV-ASSIST.
 DATE-WRITTEN.             2019/12/25.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     YA          IS        YA
     YB          IS        YB
     YA-22       IS        YA-22
     YA-21       IS        YA-21
     YA-12       IS        YA-12
     CONSOLE     IS        CONS.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*条件ファイル
     SELECT  HJYOKEN  ASSIGN        TO  JYOKEN1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  JYO-F01
                                        JYO-F02
                      FILE   STATUS IS  JYO-ST.
*担当者マスタ
     SELECT  HTANMS   ASSIGN        TO  TANMS1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  TAN-F01
                                        TAN-F02
                      FILE   STATUS IS  TAN-ST.
* プリンター
     SELECT  PRTF     ASSIGN        TO  LP-04-PRTF.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*条件ファイル
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD.
*###   COPY   HJYOKEN
*###          DISJOINING  XXX  JOINING  JYO  AS PREFIX.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*担当者マスタ
 FD  HTANMS
     LABEL       RECORD    IS        STANDARD.
*###   COPY   HTANMS
*###          DISJOINING  XXX  JOINING  TAN  AS PREFIX.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*プリンター
 FD    PRTF      LINAGE  IS  66.
 01    P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
 01  FILE-STATUS.
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  TAN-ST            PIC X(02).

 01  HJYOKEN-INV-FLG       PIC  X(03) VALUE SPACE.
 01  HTANMS-INV-FLG        PIC  X(03) VALUE SPACE.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC  9(08)  VALUE  ZERO.
     03  SYS-TIME   REDEFINES  WK-TIME.
         05  SYS-HH        PIC  9(02).
         05  SYS-MN        PIC  9(02).
         05  SYS-SS        PIC  9(02).
         05  SYS-MS        PIC  9(02).
 01  DATE-AREA.
     03  WK-YS             PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y          PIC  9(02)  VALUE  ZERO.
         05  WK-M          PIC  9(02)  VALUE  ZERO.
         05  WK-D          PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2  REDEFINES   DATE-AREA.
     03  SYS-DATE          PIC  9(08).
 01  DATE-AREAR3  REDEFINES   DATE-AREA.
     03  WK-YYYY           PIC  9(04).
     03  FILLER            PIC  X(04).
*
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  DND-ERR-NM        PIC N(10) VALUE
                        NC"受信Ｄ管理Ｆエラー".
     03  JYO-ERR-NM        PIC N(10) VALUE
                        NC"条件ファイルエラー".
     03  TAN-ERR-NM        PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  PRT-ERR-NM        PIC N(10) VALUE
                        NC"プリンターエラー".

 01  IX1                   PIC  9(02).
 01  IX-MAX                PIC  9(02).
 01  WK-AREA.
     03  WK-X-AREA         PIC  X(10).
     03  WK-X-AREA-R       REDEFINES   WK-X-AREA.
         05  WK-X          PIC  X(01)  OCCURS 10.
     03  WK-N-AREA         PIC  N(10).
     03  WK-N-AREA-R       REDEFINES   WK-N-AREA.
         05  WK-N          PIC  N(01)  OCCURS 10.

*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER                    CHARACTER  TYPE  IS  YA.
         05  FILLER        PIC  X(01)  VALUE  SPACE.
         05  FILLER        PIC  X(08)  VALUE  "NVD0130L".
         05  FILLER        PIC  X(105) VALUE  SPACE.
         05  SYSYY         PIC  9999.
         05  FILLER        PIC  N(01)  VALUE  NC"年".
         05  SYSMM         PIC  99.
         05  FILLER        PIC  N(01)  VALUE  NC"月".
         05  SYSDD         PIC  99.
         05  FILLER        PIC  N(01)  VALUE  NC"日".
         05  FILLER        PIC  X(02)  VALUE  SPACE.
         05  LPAGE         PIC  ZZ9.
         05  FILLER        PIC  N(01)  VALUE  NC"頁".
****  見出し行２***
 01  MIDASI-2.
     03  FILLER                    CHARACTER  TYPE  IS  YA-22.
         05  FILLER        PIC  X(36)  VALUE  SPACE.
         05  FILLER        PIC  N(15)  VALUE
         NC"＜Ｄ３６５連携取込件数リスト＞".
         05  FILLER        PIC  X(20)  VALUE  SPACE.
     03  FILLER                    CHARACTER  TYPE  IS  YA.
         05  TIMEHH        PIC  99.
         05  FILLER        PIC  N(01)  VALUE  NC"：".
         05  TIMEMM        PIC  99.
         05  FILLER        PIC  N(01)  VALUE  NC"：".
         05  TIMESS        PIC  99.
****  線１           ****
 01  HASEN-1.
     03  FILLER         PIC  X(136) VALUE  ALL "-".
****  線２           ****
 01  HASEN-2.
     03  FILLER         PIC  X(27) VALUE  SPACE.
     03  FILLER         PIC  X(39) VALUE  ALL "-".

****  明細行         ****
 01  MEISAI-1.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"　実　行　_　：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-SOJNO      PIC  N(10).
 01  MEISAI-2.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"連携データ種別：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-SYUBETU    PIC  N(20).
 01  MEISAI-3.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"連　携　日　付：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-DATE-YY    PIC  N(04).
         05   FILLER         PIC  N(01)  VALUE  NC"年".
         05   MEI-DATE-MM    PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"月".
         05   MEI-DATE-DD    PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"日".
 01  MEISAI-4.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"連　携　時　刻：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-TIME-HH    PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"：".
         05   MEI-TIME-MM    PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"：".
         05   MEI-TIME-SS    PIC  N(02).
 01  MEISAI-5.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"　担　当　者　：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-TANCD      PIC  X(02).
         05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-TANNM      PIC  N(10).
 01  MEISAI-6.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"　　件　数　　：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-CNT-ALL    PIC  N(10).
 01  MEISAI-7.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"　　（内ＯＫ）：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-CNT-OK     PIC  N(10).
 01  MEISAI-8.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"　　（内ＮＧ）：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-CNT-NG     PIC  N(10).
 01  MEISAI-9.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
         05   FILLER         PIC  X(37)  VALUE  SPACE.
         05   FILLER         PIC  N(08)  VALUE
         NC"　　結　果　　：".
         05   FILLER         PIC  X(01)  VALUE  SPACE.
         05   MEI-KEKA       PIC  X(04).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  PARA-SOJNO              PIC 9(10).
 01  PARA-SYUBETU            PIC X(02).
 01  PARA-TORIDATE           PIC 9(08).
 01  PARA-TORITIME           PIC 9(06).
 01  PARA-BUMON              PIC X(04).
 01  PARA-TANTO              PIC X(02).
 01  PARA-CNT-ALL            PIC 9(07).
 01  PARA-CNT-OK             PIC 9(07).
 01  PARA-CNT-NG             PIC 9(07).
 01  PARA-KEKA               PIC X(04).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-SOJNO
                                      PARA-SYUBETU
                                      PARA-TORIDATE
                                      PARA-TORITIME
                                      PARA-BUMON
                                      PARA-TANTO
                                      PARA-CNT-ALL
                                      PARA-CNT-OK
                                      PARA-CNT-NG
                                      PARA-KEKA.
******************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR-NM   UPON      CONS.
     DISPLAY     PRT-ST       UPON      CONS.
     ACCEPT      IN-DATA      FROM      CONS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR-NM   UPON      CONS.
     DISPLAY     JYO-ST       UPON      CONS.
     ACCEPT      IN-DATA      FROM      CONS.
     MOVE        4000         TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR-NM   UPON      CONS.
     DISPLAY     TAN-ST       UPON      CONS.
     ACCEPT      IN-DATA      FROM      CONS.
     MOVE        4000         TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC.
     PERFORM     END-SEC.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
*
*#### TEST <<<<<<
*### DISPLAY " 送受信指示Ｎｏ -- "  PARA-SOJNO    UPON CONS.
*### DISPLAY " 種別 ------------ "  PARA-SYUBETU  UPON CONS.
*### DISPLAY " 取込日付 -------- "  PARA-TORIDATE UPON CONS.
*### DISPLAY " 取込時刻 -------- "  PARA-TORITIME UPON CONS.
*### DISPLAY " 部門 ------------ "  PARA-BUMON    UPON CONS.
*### DISPLAY " 担当者 ---------- "  PARA-TANTO    UPON CONS.
*### DISPLAY " 件数 ------------ "  PARA-CNT-ALL  UPON CONS.
*### DISPLAY "   ＯＫ----------- "  PARA-CNT-OK   UPON CONS.
*### DISPLAY "   ＮＧ----------- "  PARA-CNT-NG   UPON CONS.
*### DISPLAY " 結果 ------------ "  PARA-KEKA     UPON CONS.
*### DISPLAY "  " UPON CONS.
*#### TEST >>>>>>

*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN        INPUT     HJYOKEN
                           HTANMS.
     OPEN        OUTPUT    PRTF.
*
     DISPLAY  "*** NVD0130L START *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     IF   PARA-SOJNO  =  ZERO
      OR  PARA-SOJNO  =  SPACE
          DISPLAY "＃＃パラメタ送受信Ｎｏエラー" UPON CONS
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
*ヘッダー／項目編集
     PERFORM  HEAD-SET-SEC.

*明細／項目編集
     PERFORM  MEISAI-SET-SEC.

*ヘッダー／プリント
     MOVE       SPACE    TO      P-REC.
     WRITE      P-REC                         AFTER   2.
     WRITE      P-REC    FROM    MIDASI-1     AFTER   1.
     WRITE      P-REC    FROM    MIDASI-2     AFTER   1.
     WRITE      P-REC    FROM    HASEN-1      AFTER   2.

*明細／プリント
     WRITE      P-REC    FROM    MEISAI-1     AFTER   3.
     WRITE      P-REC    FROM    MEISAI-2     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-3     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-4     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-5     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-6     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-7     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-8     AFTER   4.
     WRITE      P-REC    FROM    MEISAI-9     AFTER   4.

*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF     HJYOKEN   HTANMS.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     DISPLAY  "*** NVD0130L END   *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-EXIT.
     EXIT.
**********************************************************
*    ヘッダ部／編集
**********************************************************
 HEAD-SET-SEC                  SECTION.
*
     MOVE    WK-YYYY       TO    SYSYY.
     MOVE    WK-M          TO    SYSMM.
     MOVE    WK-D          TO    SYSDD.
     MOVE    WK-TIME(1:2)  TO    TIMEHH.
     MOVE    WK-TIME(3:2)  TO    TIMEMM.
     MOVE    WK-TIME(5:2)  TO    TIMESS.
     MOVE    1             TO    LPAGE.
*
 HEAD-SET-EXIT.
     EXIT.
**********************************************************
*    明細部／編集
**********************************************************
 MEISAI-SET-SEC            SECTION.

*  送受信指示Ｎｏ
     MOVE   10                  TO   IX-MAX.
     MOVE   PARA-SOJNO          TO   WK-X-AREA.
     PERFORM  ZENKAKU-SEC.
     MOVE   WK-N-AREA           TO   MEI-SOJNO.

*  連携データ種別
     MOVE  87                   TO   JYO-F01.
     MOVE  PARA-SYUBETU         TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG  =  SPACE
         MOVE  JYO-F03          TO   MEI-SYUBETU
     ELSE
         MOVE  ALL NC"？"       TO   MEI-SYUBETU
     END-IF.

*  連携日付
     MOVE   8                   TO   IX-MAX.
     MOVE   PARA-TORIDATE       TO   WK-X-AREA.
     IF  WK-X(5) = ZERO
         MOVE   SPACE           TO   WK-X(5)
     END-IF.
     IF  WK-X(7) = ZERO
         MOVE   SPACE           TO   WK-X(7)
     END-IF.
     PERFORM  ZENKAKU-SEC.
     MOVE   WK-N-AREA(1:4)      TO   MEI-DATE-YY.
     MOVE   WK-N-AREA(5:2)      TO   MEI-DATE-MM.
     MOVE   WK-N-AREA(7:2)      TO   MEI-DATE-DD.

*  連携時刻
     MOVE   6                   TO   IX-MAX.
     MOVE   PARA-TORITIME       TO   WK-X-AREA.
     PERFORM  ZENKAKU-SEC.
     MOVE   WK-N-AREA(1:2)      TO   MEI-TIME-HH.
     MOVE   WK-N-AREA(3:2)      TO   MEI-TIME-MM.
     MOVE   WK-N-AREA(5:2)      TO   MEI-TIME-SS.

*  担当者
     MOVE  PARA-TANTO           TO   MEI-TANCD.
     MOVE  PARA-BUMON           TO   TAN-F01.
     MOVE  PARA-TANTO           TO   TAN-F02.
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG  =  SPACE
         MOVE  TAN-F03          TO   MEI-TANNM
     ELSE
         MOVE  ALL NC"？"       TO   MEI-TANNM
     END-IF.

*  件数（全件）
     MOVE   10                  TO   IX-MAX.
     MOVE   "00,000,000"        TO   WK-X-AREA.
     MOVE   PARA-CNT-ALL(1:1)   TO   WK-X-AREA(2:1).
     MOVE   PARA-CNT-ALL(2:3)   TO   WK-X-AREA(4:3).
     MOVE   PARA-CNT-ALL(5:3)   TO   WK-X-AREA(8:3).
     PERFORM  VARYING IX1 FROM 1 BY 1  UNTIL IX1 >= IX-MAX
        IF   WK-X(IX1)  =  "0"  OR  ","
             MOVE   SPACE       TO   WK-X(IX1)
        ELSE
             MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE   WK-N-AREA           TO   MEI-CNT-ALL.

*  件数（ＯＫ）
     MOVE   10                  TO   IX-MAX.
     MOVE   "00,000,000"        TO   WK-X-AREA.
     MOVE   PARA-CNT-OK(1:1)    TO   WK-X-AREA(2:1).
     MOVE   PARA-CNT-OK(2:3)    TO   WK-X-AREA(4:3).
     MOVE   PARA-CNT-OK(5:3)    TO   WK-X-AREA(8:3).
     PERFORM  VARYING IX1 FROM 1 BY 1  UNTIL IX1 >= IX-MAX
        IF   WK-X(IX1)  =  "0"  OR  ","
             MOVE   SPACE       TO   WK-X(IX1)
        ELSE
             MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE   WK-N-AREA           TO   MEI-CNT-OK.

*  件数（ＮＧ）
     MOVE   10                  TO   IX-MAX.
     MOVE   "00,000,000"        TO   WK-X-AREA.
     MOVE   PARA-CNT-NG(1:1)    TO   WK-X-AREA(2:1).
     MOVE   PARA-CNT-NG(2:3)    TO   WK-X-AREA(4:3).
     MOVE   PARA-CNT-NG(5:3)    TO   WK-X-AREA(8:3).
     PERFORM  VARYING IX1 FROM 1 BY 1  UNTIL IX1 >= IX-MAX
        IF   WK-X(IX1)  =  "0"  OR  ","
             MOVE   SPACE       TO   WK-X(IX1)
        ELSE
             MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE   WK-N-AREA           TO   MEI-CNT-NG.

*  結果
     MOVE  PARA-KEKA            TO   MEI-KEKA.
*
 MEISAI-SET-EXIT.
     EXIT.
****************************************************************
*    条件マスタ／検索
****************************************************************
 HJYOKEN-READ-SEC       SECTION.

     READ  HJYOKEN
       INVALID
         MOVE  "INV"             TO  HJYOKEN-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  HJYOKEN-INV-FLG
     END-READ.

 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ／検索
****************************************************************
 HTANMS-READ-SEC        SECTION.

     READ  HTANMS
       INVALID
         MOVE  "INV"             TO  HTANMS-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  HTANMS-INV-FLG
     END-READ.

 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*    全角変換
****************************************************************
 ZENKAKU-SEC          SECTION.

     MOVE   SPACE          TO   WK-N-AREA.
* 日本語変換する。
     PERFORM  VARYING  IX1 FROM 1 BY 1  UNTIL IX1 > IX-MAX
         EVALUATE    WK-X(IX1)
             WHEN    "0"
                     MOVE NC"０"  TO   WK-N(IX1)
             WHEN    "1"
                     MOVE NC"１"  TO   WK-N(IX1)
             WHEN    "2"
                     MOVE NC"２"  TO   WK-N(IX1)
             WHEN    "3"
                     MOVE NC"３"  TO   WK-N(IX1)
             WHEN    "4"
                     MOVE NC"４"  TO   WK-N(IX1)
             WHEN    "5"
                     MOVE NC"５"  TO   WK-N(IX1)
             WHEN    "6"
                     MOVE NC"６"  TO   WK-N(IX1)
             WHEN    "7"
                     MOVE NC"７"  TO   WK-N(IX1)
             WHEN    "8"
                     MOVE NC"８"  TO   WK-N(IX1)
             WHEN    "9"
                     MOVE NC"９"  TO   WK-N(IX1)
             WHEN    " "
                     MOVE NC"　"  TO   WK-N(IX1)
             WHEN    ","
                     MOVE NC"，"  TO   WK-N(IX1)
             WHEN    OTHER
                     MOVE NC"　"  TO   WK-N(IX1)
         END-EVALUATE
     END-PERFORM.
 ZENKAKU-EXIT.
     EXIT.

```
