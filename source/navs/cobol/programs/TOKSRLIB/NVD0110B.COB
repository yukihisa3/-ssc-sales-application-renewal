****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定データ伝票単位更新　　　　*
*    作成日／作成者　　　：　2019/12/25   ASS.TAKAHASHI        *
*    処理内容　　　　　　：　入荷予定取込データを伝票単位に　　*
*    　　　　　　　　　　　　エラー区分を更新する。　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0110B.
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
     SELECT  DNDTKNF  ASSIGN        TO  DNDTKNL2
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  SEQUENTIAL
                      RECORD KEY    IS  DND-F01 *> 送受信指示NO
                                        DND-F07 *> 取込日付
                                        DND-F08 *> 取込時刻
                                        DND-F09 *> 伝票番号
                                        DND-F22 *> エラー区分
                                        DND-F11 *> ＮＡＶＳ採番
                                        DND-F10 *> 行番号
*****                 WITH  DUPLICATES.
                      FILE   STATUS IS  DND-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*ＮＡＶＳ受信データ管理ファイル
 FD  DNDTKNF.
*###   COPY   DNDTKNF
*###          DISJOINING  XXX  JOINING  DND  AS PREFIX.
     COPY        DNDTKNF     OF        XFDLIB
     JOINING     DND         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  DND-ST              PIC  X(02)  VALUE  SPACE.

 01  END-FLG                 PIC  X(03)  VALUE  SPACE.

 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)  VALUE  ZERO.
     03  UPD-CNT             PIC  9(07)  VALUE  ZERO.

 01  WK-AREA.
     03  WK-NAVS-DENNO       PIC  X(10).
     03  WK-ERR-KBN          PIC  X(01).

*日付取得
 01  SYS-DATE                PIC  9(06).
 01  WK-DATE8.
     03  WK-Y                PIC  9(04).
     03  WK-M                PIC  9(02).
     03  WK-D                PIC  9(02).
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
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
         05  ST-PG           PIC  X(08)  VALUE "NVD0110B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVD0110B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 受信Ｄ管理Ｆ　　読込件数 = ".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 受信Ｄ管理Ｆ　　更新件数 = ".
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
 01  PARA-SOJNO              PIC  9(10).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-SOJNO.
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

*#### TEST <<<<<<
*### DISPLAY " 送受信指示Ｎｏ --- "  PARA-SOJNO    UPON CONS.
*### DISPLAY "  " UPON CONS.
*#### TEST >>>>>>
*
     DISPLAY     MSG-START    UPON  CONS.
*
     OPEN        I-O          DNDTKNF.

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
*ＮＡＶＳ受信データ管理ファイルを読み込む
     INITIALIZE                        DND-REC.
     MOVE     PARA-SOJNO          TO   DND-F01.
     START    DNDTKNF   KEY >=    DND-F01
                                  DND-F07
                                  DND-F08
                                  DND-F09
                                  DND-F22
                                  DND-F11
                                  DND-F10
          INVALID KEY
              MOVE  "END"       TO   END-FLG
          NOT INVALID
              PERFORM  DNDTKNF-READ-SEC
     END-START.
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
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

*ＮＡＶＳ採番伝票番号を判定
     IF  DND-F11   NOT =   WK-NAVS-DENNO
         MOVE  DND-F11            TO  WK-NAVS-DENNO
         IF  DND-F22   =   "1"
             MOVE  "1"            TO  WK-ERR-KBN
         ELSE
             MOVE  SPACE          TO  WK-ERR-KBN
         END-IF
     END-IF.

*更新
     IF  WK-ERR-KBN    =   "1"
         MOVE  "1"                TO  DND-F22
         REWRITE  DND-REC
         ADD   1                  TO  UPD-CNT
     END-IF.

*次のＮＡＶＳ受信データ管理ファイルを読み込む
     PERFORM  DNDTKNF-READ-SEC.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*    ＮＡＶＳ受信データ管理ファイル　順読込
****************************************************************
 DNDTKNF-READ-SEC            SECTION.
     MOVE     "DNDTKNF-READ-SEC"  TO   S-NAME.
*
     READ     DNDTKNF
         AT END
              MOVE   "END"        TO   END-FLG
              GO      TO          DNDTKNF-READ-EXIT
     END-READ.

     IF  DND-F01  NOT =  PARA-SOJNO
              MOVE   "END"        TO   END-FLG
              GO      TO          DNDTKNF-READ-EXIT
     END-IF.
*
     ADD      1   TO    IN-CNT.
*
 DNDTKNF-READ-EXIT.
     EXIT.
****************************************************************
*    終了処理
****************************************************************
 END-SEC                   SECTION.
     MOVE     "END-SEC"         TO   S-NAME.
*
     MOVE     IN-CNT            TO   MSG-OUT01.
     MOVE     UPD-CNT           TO   MSG-OUT02.
     DISPLAY  MSG-OUT1        UPON   CONS.
     DISPLAY  MSG-OUT2        UPON   CONS.
*
     CLOSE    DNDTKNF.

     DISPLAY  MSG-END         UPON   CONS.

 END-EXIT.
     EXIT.
