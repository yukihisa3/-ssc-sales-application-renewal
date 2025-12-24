# NVD0600L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0600L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　
*    モジュール名　　　　：　日次計上件数リスト
*    作成日／作成者　　　：　2022/02/25 INOUE
*    処理概要　　　　　　：　パラメタに合致するレコードより
*    　　　　　　　　　　　　件数リストを出力する。
*    更新履歴　　　　　　：  受注売上値引件数追加
*    　　　　　　　　　　　
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               NVD0600L.
*                  流用元：NVD0600L.TOKSRLIB
 AUTHOR.                   NAV-ASSIST.
 DATE-WRITTEN.             2022/02/25.
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
*件数ファイル
     SELECT  DNITKNF  ASSIGN        TO  DNITKNL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  SEQUENTIAL
                      RECORD KEY    IS  DNI-F01
                                        DNI-F02
                      FILE   STATUS IS  DNI-ST.
* プリンター
     SELECT  PRTF     ASSIGN        TO  LP-04-PRTF.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*件数ファイル
 FD  DNITKNF
     LABEL       RECORD    IS        STANDARD.
     COPY        DNITKNF   OF        XFDLIB
     JOINING     DNI       AS        PREFIX.
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
     03  DNI-ST            PIC X(02).

 01  END-FLG               PIC  X(03) VALUE SPACE.
 01  DNI-INV-FLG           PIC  X(03) VALUE SPACE.
 01  DNI-END-FLG           PIC  X(03) VALUE SPACE.
 01  DNI-READ-CNT          PIC  9(05) VALUE ZERO.
 01  P-CNT                 PIC  9(03) VALUE 1.
 01  L-CNT                 PIC  9(02) VALUE ZERO.
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
     03  DNI-ERR-NM        PIC N(10) VALUE
                        NC"件数ファイルエラー".
     03  PRT-ERR-NM        PIC N(10) VALUE
                        NC"プリンターエラー".

 01  IX1                   PIC  9(02).
 01  IX-MAX                PIC  9(02).
 01  WK-AREA.
     03  WK-X-AREA         PIC  X(09).
     03  WK-X-AREA-R       REDEFINES   WK-X-AREA.
         05  WK-X          PIC  X(01)  OCCURS 9.
     03  WK-N-AREA         PIC  N(09).
     03  WK-N-AREA-R       REDEFINES   WK-N-AREA.
         05  WK-N          PIC  N(01)  OCCURS 9.

*帳票出力定義エリア
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER                    CHARACTER  TYPE  IS  YA.
         05  FILLER        PIC  X(01)  VALUE  SPACE.
         05  FILLER        PIC  X(08)  VALUE  "NVD0600L".
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
         05  FILLER        PIC  X(28)  VALUE  SPACE.
         05  FILLER        PIC  N(17)  VALUE
         NC"＜Ｄ３６５連携日次計上件数リスト＞".
         05  FILLER        PIC  X(20)  VALUE  SPACE.
     03  FILLER                    CHARACTER  TYPE  IS  YA.
         05  TIMEHH        PIC  99.
         05  FILLER        PIC  N(01)  VALUE  NC"：".
         05  TIMEMM        PIC  99.
         05  FILLER        PIC  N(01)  VALUE  NC"：".
         05  TIMESS        PIC  99.
****  見出し行3***
 01  MIDASI-3.
     03  FILLER          CHARACTER  TYPE  IS  YA-12.
         05  FILLER      PIC  X(04) VALUE  SPACE.
         05  FILLER      PIC  N(07) VALUE  NC"日　次　日　付".
         05  FILLER      PIC  X(07) VALUE  SPACE.
         05  FILLER      PIC  N(07) VALUE  NC"日　次　時　刻".
         05  FILLER      PIC  X(07) VALUE  SPACE.
         05  FILLER      PIC  N(06) VALUE  NC"受注計上件数".
         05  FILLER      PIC  X(06) VALUE  SPACE.
         05  FILLER      PIC  N(06) VALUE  NC"売上計上件数".
         05  FILLER      PIC  X(06) VALUE  SPACE.
         05  FILLER      PIC  N(06) VALUE  NC"売調計上件数".
         05  FILLER      PIC  X(06) VALUE  SPACE.
         05  FILLER      PIC  N(06) VALUE  NC"受値計上件数".
         05  FILLER      PIC  X(06) VALUE  SPACE.
         05  FILLER      PIC  N(06) VALUE  NC"売値計上件数".
****  線１           ****
 01  HASEN-1.
     03  FILLER         PIC  X(136) VALUE  ALL "-".
****  線２           ****
 01  HASEN-2.
     03  FILLER         PIC  X(27) VALUE  SPACE.
     03  FILLER         PIC  X(39) VALUE  ALL "-".

****  明細行         ****
 01  MEISAI.
     03  FILLER                   CHARACTER  TYPE  IS  YA-12.
*        05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-1-YYYY     PIC  N(04).
         05   FILLER         PIC  N(01)  VALUE  NC"年".
         05   MEI-1-MM       PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"月".
         05   MEI-1-DD       PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"日".
         05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-2-HH       PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"：".
         05   MEI-2-MM       PIC  N(02).
         05   FILLER         PIC  N(01)  VALUE  NC"：".
         05   MEI-2-SS       PIC  N(02).
*        05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-3-KEN      PIC  N(09).
*        05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-4-KEN      PIC  N(09).
*        05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-5-KEN      PIC  N(09).
*        05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-6-KEN      PIC  N(09).
*        05   FILLER         PIC  X(02)  VALUE  SPACE.
         05   MEI-7-KEN      PIC  N(09).
*
*対象データなし
*01  LST-DATA-X.
*    03  FILLER         CHARACTER TYPE YB-21.
*        05  FILLER          PIC  X(25)     VALUE  SPACE.
*        05  FILLER          PIC  N(22)     VALUE
*            NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
*01  LST-DATA-Y.
*    03  FILLER         CHARACTER TYPE YB-21.
*        05  FILLER          PIC  X(25)     VALUE  SPACE.
*        05  FILLER          PIC  N(22)     VALUE
*            NC"＃　　　　対象データはありません。　　　　＃".
*01  LST-DATA-Z.
*    03  FILLER         CHARACTER TYPE YB-21.
*        05  FILLER          PIC  X(25)     VALUE  SPACE.
*        05  FILLER          PIC  N(22)     VALUE
*            NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                   SECTION.
 01  PARA-DATE               PIC 9(08).
 01  PARA-TIME               PIC 9(06).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-DATE
                                      PARA-TIME.
******************************************************************
 DECLARATIVES.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTF.
     DISPLAY     PRT-ERR-NM   UPON      CONS.
     DISPLAY     PRT-ST       UPON      CONS.
     ACCEPT      IN-DATA      FROM      CONS.
     STOP        RUN.
 DNI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNITKNF.
     DISPLAY     DNI-ERR-NM   UPON      CONS.
     DISPLAY     DNI-ST       UPON      CONS.
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
     PERFORM     MAIN-SEC  UNTIL  END-FLG = "END".
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
     DISPLAY " 日次日付 -- "  PARA-DATE     UPON CONS.
     DISPLAY " 日次時刻 -- "  PARA-TIME     UPON CONS.
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
     OPEN        INPUT     DNITKNF.
     OPEN        OUTPUT    PRTF.
*
     DISPLAY  "*** NVD0600L START *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     IF   PARA-DATE  =  ZERO
      OR  PARA-DATE     NOT NUMERIC
          DISPLAY "＃＃パラメタ連携日付エラー" UPON CONS
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-IF.
*
*    件数ファイル初期ＲＥＡＤ
     PERFORM  DNITKNF-START-SEC.
     IF       DNI-INV-FLG = "INV"
         DISPLAY NC"＃＃　対象データ　なし　１＃＃" UPON CONS
*        WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
*        WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
*        WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
*        WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
*        WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
         MOVE    "END"         TO    END-FLG
         GO                    TO    INIT-EXIT
     END-IF.
*
     PERFORM  DNITKNF-READ-SEC.
     IF       DNI-END-FLG = "END"
         DISPLAY NC"＃＃　対象データ　なし　２＃＃" UPON CONS
*        WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
*        WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
*        WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
*        WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
*        WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
         MOVE    "END"         TO    END-FLG
         GO                    TO    INIT-EXIT
     END-IF.
*
*ヘッダー／項目編集
     PERFORM  HEAD-SET-SEC.
*ヘッダー／プリント
     MOVE       SPACE    TO      P-REC.
     WRITE      P-REC                         AFTER   1.
     WRITE      P-REC    FROM    MIDASI-1     AFTER   1.
     WRITE      P-REC    FROM    MIDASI-2     AFTER   1.
     WRITE      P-REC    FROM    HASEN-1      AFTER   2.
     WRITE      P-REC    FROM    MIDASI-3     AFTER   2.
     WRITE      P-REC    FROM    HASEN-1      AFTER   1.
     ADD        9        TO      L-CNT.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*
*改ページ
     IF       L-CNT   >  50
              ADD     1       TO      P-CNT
              MOVE    P-CNT   TO      LPAGE
              MOVE    SPACE   TO      P-REC
              WRITE   P-REC                        AFTER   PAGE
              WRITE   P-REC   FROM    MIDASI-1     AFTER   1
              WRITE   P-REC   FROM    MIDASI-2     AFTER   1
              WRITE   P-REC   FROM    HASEN-1      AFTER   2
              WRITE   P-REC   FROM    MIDASI-3     AFTER   2
              WRITE   P-REC   FROM    HASEN-1      AFTER   1
              MOVE    9       TO      L-CNT

     END-IF.
*
*明細／項目編集
     PERFORM  MEISAI-SET-SEC.
*
*明細／プリント
     WRITE    P-REC    FROM    MEISAI       AFTER   2.
     ADD      2        TO      L-CNT.
*
     PERFORM  DNITKNF-READ-SEC.
     IF       DNI-END-FLG = "END"
              MOVE    "END"         TO    END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
*
     CLOSE       PRTF     DNITKNF.
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     DISPLAY  "*** NVD0600L END   *** "
              WK-Y   "." WK-M   "." WK-D   " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
* 件数ファイル スタート
****************************************************************
 DNITKNF-START-SEC           SECTION.
*
*    MOVE    "DNITKNF-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   DNI-REC.
     INITIALIZE                        DNI-REC.
*
     MOVE     PARA-DATE           TO   DNI-F01.
     MOVE     PARA-TIME           TO   DNI-F02.
*
     START  DNITKNF   KEY  IS  >=      DNI-F01  DNI-F02
            INVALID
            MOVE    "INV"         TO   DNI-INV-FLG
     END-START.
*
 DNITKNF-START-EXIT.
     EXIT.
*
****************************************************************
* 件数ファイル   読込
****************************************************************
 DNITKNF-READ-SEC           SECTION.
*
*    MOVE    "DNITKNF-READ-SEC"   TO   S-NAME.
*
     READ     DNITKNF  AT  END
              MOVE     "END"      TO   DNI-END-FLG
              GO                  TO   DNITKNF-READ-EXIT
     END-READ.
*対象判定
     IF  DNI-F01   =  PARA-DATE
         IF   PARA-TIME =  ZERO
              CONTINUE
         ELSE
              IF  DNI-F02  = PARA-TIME
                  CONTINUE
              ELSE
                  MOVE     "END"      TO   DNI-END-FLG
                  GO                  TO   DNITKNF-READ-EXIT
              END-IF
         END-IF
     ELSE
              MOVE     "END"      TO   DNI-END-FLG
              GO                  TO   DNITKNF-READ-EXIT
     END-IF.
*
*件数カウント
     ADD      1                   TO   DNI-READ-CNT.
*
 DNITKNF-READ-EXIT.
     EXIT.
*
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
*
*  連携日付
     MOVE     8                   TO   IX-MAX.
     MOVE     DNI-F01             TO   WK-X-AREA.
     IF       WK-X(5) = ZERO
              MOVE   SPACE        TO   WK-X(5)
     END-IF.
     IF       WK-X(7) = ZERO
              MOVE   SPACE        TO   WK-X(7)
     END-IF.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA(1:4)      TO   MEI-1-YYYY.
     MOVE     WK-N-AREA(5:2)      TO   MEI-1-MM.
     MOVE     WK-N-AREA(7:2)      TO   MEI-1-DD.
*
*  連携時刻
     MOVE     6                   TO   IX-MAX.
     MOVE     DNI-F02             TO   WK-X-AREA.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA(1:2)      TO   MEI-2-HH.
     MOVE     WK-N-AREA(3:2)      TO   MEI-2-MM.
     MOVE     WK-N-AREA(5:2)      TO   MEI-2-SS.
*
*  受注計上件数
     MOVE     9                  TO   IX-MAX.
     MOVE     "0,000,000"        TO   WK-X-AREA.
     MOVE     DNI-F03(1:1)       TO   WK-X-AREA(1:1).
     MOVE     DNI-F03(2:3)       TO   WK-X-AREA(3:3).
     MOVE     DNI-F03(5:3)       TO   WK-X-AREA(7:3).
     PERFORM  VARYING IX1 FROM 1 BY 1 UNTIL IX1 >= IX-MAX
        IF    WK-X(IX1)  =  "0"  OR  ","
              MOVE   SPACE       TO   WK-X(IX1)
        ELSE
              MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA          TO   MEI-3-KEN.
*
*  売上計上件数
     MOVE     9                  TO   IX-MAX.
     MOVE     "0,000,000"        TO   WK-X-AREA.
     MOVE     DNI-F04(1:1)       TO   WK-X-AREA(1:1).
     MOVE     DNI-F04(2:3)       TO   WK-X-AREA(3:3).
     MOVE     DNI-F04(5:3)       TO   WK-X-AREA(7:3).
     PERFORM  VARYING IX1 FROM 1 BY 1 UNTIL IX1 >= IX-MAX
        IF    WK-X(IX1)  =  "0"  OR  ","
              MOVE   SPACE       TO   WK-X(IX1)
        ELSE
              MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA          TO   MEI-4-KEN.
*
*  売上調整計上件数
     MOVE     9                  TO   IX-MAX.
     MOVE     "0,000,000"        TO   WK-X-AREA.
     MOVE     DNI-F05(1:1)       TO   WK-X-AREA(1:1).
     MOVE     DNI-F05(2:3)       TO   WK-X-AREA(3:3).
     MOVE     DNI-F05(5:3)       TO   WK-X-AREA(7:3).
     PERFORM  VARYING IX1 FROM 1 BY 1 UNTIL IX1 >= IX-MAX
        IF    WK-X(IX1)  =  "0"  OR  ","
              MOVE   SPACE       TO   WK-X(IX1)
        ELSE
              MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA          TO   MEI-5-KEN.
*
*  受注値引計上件数
     MOVE     9                  TO   IX-MAX.
     MOVE     "0,000,000"        TO   WK-X-AREA.
     MOVE     DNI-F06(1:1)       TO   WK-X-AREA(1:1).
     MOVE     DNI-F06(2:3)       TO   WK-X-AREA(3:3).
     MOVE     DNI-F06(5:3)       TO   WK-X-AREA(7:3).
     PERFORM  VARYING IX1 FROM 1 BY 1 UNTIL IX1 >= IX-MAX
        IF    WK-X(IX1)  =  "0"  OR  ","
              MOVE   SPACE       TO   WK-X(IX1)
        ELSE
              MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA          TO   MEI-6-KEN.
*
*  売上値引計上件数
     MOVE     9                  TO   IX-MAX.
     MOVE     "0,000,000"        TO   WK-X-AREA.
     MOVE     DNI-F07(1:1)       TO   WK-X-AREA(1:1).
     MOVE     DNI-F07(2:3)       TO   WK-X-AREA(3:3).
     MOVE     DNI-F07(5:3)       TO   WK-X-AREA(7:3).
     PERFORM  VARYING IX1 FROM 1 BY 1 UNTIL IX1 >= IX-MAX
        IF    WK-X(IX1)  =  "0"  OR  ","
              MOVE   SPACE       TO   WK-X(IX1)
        ELSE
              MOVE   IX-MAX      TO   IX1
        END-IF
     END-PERFORM.
     PERFORM  ZENKAKU-SEC.
     MOVE     WK-N-AREA          TO   MEI-7-KEN.
*
 MEISAI-SET-EXIT.
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
