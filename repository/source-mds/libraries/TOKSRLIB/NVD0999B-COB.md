# NVD0999B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0999B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　ＮＡＶＳ受信ＤＴ管理Ｆ削除　　　　*
*    作成日／作成者　　　：　2023/06/01   NAV TAKAHASHI        *
*    処理内容　　　　　　：　ＮＡＶＳ受信ＤＴ管理Ｆの削除を　　*
*    　　　　　　　　　　　　行なう。　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0999B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*ＮＡＶＳ受信データ管理ファイル
     SELECT  DNDTKNF  ASSIGN        TO  DNDTKNL1
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  SEQUENTIAL
                      RECORD KEY    IS  DND-F01 *> 送受信指示NO
                                        DND-F07 *> 取込日付
                                        DND-F08 *> 取込時刻
                                        DND-F09 *> 伝票番号
                                        DND-F10 *> 行番号
                      FILE   STATUS IS  DND-ST.
D***************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ＮＡＶＳ受信データ管理ファイル
 FD  DNDTKNF.
     COPY        DNDTKNF     OF        XFDLIB
     JOINING     DND         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  DND-ST              PIC  X(02)  VALUE  SPACE.

 01  END-FLG                 PIC  X(03)  VALUE  SPACE.

 01  CNT-AREA.
     03  DEL-CNT             PIC  9(07)  VALUE  ZERO.
     03  DND-CNT             PIC  9(07)  VALUE  ZERO.
     03  OK-CNT              PIC  9(07)  VALUE  ZERO.
     03  NG-CNT              PIC  9(07)  VALUE  ZERO.
 01  FLG-AREA.
     03  SUBMEIF-INV-FLG     PIC  X(03).
     03  ZSOKMS-INV-FLG      PIC  X(03).
     03  ZSHIMS-INV-FLG      PIC  X(03).
     03  JYOKEN-INV-FLG      PIC  X(03).
     03  DNDTKNF-INV-FLG     PIC  X(03).

 01  WK-AREA.
     03  WK-DENNO            PIC  X(20).
     03  WK-NAVS-DENNO       PIC  X(10).
     03  WK-NAVS-DENNO-R          REDEFINES WK-NAVS-DENNO.
         05  WK-NAVS         PIC  9(10).
     03  WK-NAVS-MIN         PIC  9(10).
     03  WK-NAVS-MAX         PIC  9(10).

 01  CHK-AREA.
     03  CHK-ERR-KBN         PIC  X(01).
     03  CHK-ERR             PIC  X(01)  OCCURS 10.

*日付取得
 01  SYS-DATE                PIC  9(06).
 01  WK-DATE8.
     03  WK-Y                PIC  9(04).
     03  WK-M                PIC  9(02).
     03  WK-D                PIC  9(02).
 01  SYS-TIME.
     03  WK-TIME             PIC  9(06).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  DND-ERR-NM          PIC  N(10)  VALUE
                   NC"受信データ管理Ｆ異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVD0999B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVD0999B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " ＮＡＶＳ管理ＤＴ読込件数 = ".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " ＮＡＶＳ管理ＤＴ削除件数 = ".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  PARA-ST-SOJNO           PIC  9(10).
 01  PARA-ED-SOJNO           PIC  9(10).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-ST-SOJNO
                                      PARA-ED-SOJNO.
****************************************************************
 DECLARATIVES.
 DND-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DNDTKNF.
     DISPLAY     DND-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DND-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*    初期処理
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     DISPLAY     MSG-START    UPON  CONS.
*
     OPEN        I-O         DNDTKNF.

*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.

     INITIALIZE             WK-AREA.
*
     MOVE     SPACE          TO    DND-REC.
     INITIALIZE                    DND-REC.
     MOVE     PARA-ST-SOJNO  TO    DND-F01.
     START  DNDTKNF KEY IS  >=     DND-F01  DND-F07  DND-F08
                                   DND-F09  DND-F10
            INVALID
            DISPLAY "＃＃出力対象データ無し１＃＃" UPON CONS
            MOVE    "END"     TO    END-FLG
            MOVE     4000     TO    PROGRAM-STATUS
            STOP     RUN
     END-START.
*ＮＡＶＳ受信データ管理ファイル読込
     PERFORM  DNDTKNF-READ-SEC.
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し２＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    主処理
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.

*ＮＡＶＳ受信データ管理ファイル削除
     DELETE  DNDTKNF.
     ADD     1                    TO   DEL-CNT.
*ＮＡＶＳ受信データ管理ファイル読込
     PERFORM  DNDTKNF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    ＮＡＶＳ受信データ管理ファイル
****************************************************************
 DNDTKNF-READ-SEC            SECTION.
     MOVE     "DNDTKNF-READ-SEC"  TO   S-NAME.
*
     READ     DNDTKNF
         AT END
              MOVE   "END"        TO   END-FLG
              GO      TO          DNDTKNF-READ-EXIT
     END-READ.
*
     ADD      1   TO    DND-CNT.
*
     IF  DND-CNT (5:3)  =  "000" OR "500"
         DISPLAY "READ-CNT = " DND-CNT  UPON CONS
     END-IF.
*
     IF  DND-F01   >  PARA-ED-SOJNO
              MOVE   "END"        TO   END-FLG
     END-IF.
*
 DNDTKNF-READ-EXIT.
     EXIT.
****************************************************************
*    終了処理
****************************************************************
 END-SEC                   SECTION.
     MOVE     "END-SEC"         TO   S-NAME.
*
     CLOSE    DNDTKNF.
*
     MOVE     DND-CNT           TO   MSG-OUT01.
     MOVE     DEL-CNT           TO   MSG-OUT02.
     DISPLAY  MSG-OUT1 UPON CONS.
     DISPLAY  MSG-OUT2 UPON CONS.
     DISPLAY  MSG-END         UPON   CONS.
*
 END-EXIT.
     EXIT.

```
