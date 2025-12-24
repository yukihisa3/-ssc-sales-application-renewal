****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携（移動依頼）　　　　　*
*    モジュール名　　　　：　計上ＦＬＧ初期化（伝票ＮＯ範囲）　*
*    作成日／作成者　　　：　2021/08/26   NAV TAKAHASHI        *
*    処理内容　　　　　　：　計上、日付、時刻の初期化　　　
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD045R1.
 AUTHOR.                ASS.II.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<  新入出庫ファイル  >>******************************
     SELECT   DNSFILF   ASSIGN    TO      DA-01-VI-DNSFILL1
                        FILE STATUS          IS  DNS-STATUS
                        ORGANIZATION         IS  INDEXED
                        ACCESS  MODE         IS  SEQUENTIAL
                        RECORD  KEY          IS  DNS-F01
                                                 DNS-F02.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 新入出庫ファイル >>*********************************
 FD  DNSFILF.
     COPY     DNSFILF   OF        XFDLIB
              JOINING   DNS       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 DNS-STATUS           PIC  X(02).
****  システム日付・時刻
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC  9(06).
     03  WK-SYSDATE          PIC  9(08).
     03  SYSHMS.
         05  WK-SYSTIME      PIC  9(06).
****  フラグ
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** DNSFILL3   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** DNSFILL1  (INPUT) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** DSNFILL1(FLG-CLR) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD045R1".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  PARA-STDENNO            PIC  9(07).
 01  PARA-EDDENNO            PIC  9(07).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-STDENNO  PARA-EDDENNO.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DNSFILF.
     MOVE   "DNSFILF"         TO    ERR-FL-ID.
     MOVE    DNS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
                            UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理
************************************************************
 100-INIT-SEC           SECTION.

*システム日付・時刻の取得
     ACCEPT   SYSYMD     FROM     DATE.
     ACCEPT   SYSHMS     FROM     TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-SYSDATE.

     OPEN     I-O       DNSFILF.

*在庫移動ファイルスタート
     MOVE     SPACE               TO   DNS-REC.
     INITIALIZE                        DNS-REC.
     MOVE     PARA-STDENNO        TO   DNS-F01.
     START  DNSFILF KEY IS  >=  DNS-F01  DNS-F02
            INVALID
            MOVE   "END"          TO   END-FLG
            DISPLAY NC"＃対象ＤＴ無１＃" UPON CONS
            GO                    TO   100-INIT-END
     END-START.
*在庫移動ファイル読込
     PERFORM  DNSFILF-READ-SEC.
     IF  END-FLG = "END"
            DISPLAY NC"＃対象ＤＴ無２＃" UPON CONS
     END-IF.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*計上ＦＬＧ、連携日、連携時刻初期化
     MOVE     SPACE           TO   DNS-F22
     MOVE     ZERO            TO   DNS-F23  DNS-F24.
     MOVE     20210901        TO   DNS-F18.
*    MOVE     "1"             TO   DNS-F22
*    MOVE     20210805        TO   DNS-F23
*    MOVE     164957          TO   DNS-F24
     REWRITE  DNS-REC.
     ADD      1               TO   OUT-CNT.

*次の入出庫ファイルの読込
     PERFORM  DNSFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    入出庫ファイルの読込処理
************************************************************
 DNSFILF-READ-SEC       SECTION.

 DNSFILF-READ-100.
     READ    DNSFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        DNSFILF-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            MOVE   IN-CNT             TO   DSP-READ
            DISPLAY  DSP-READ-AREA  UPON   CONS
     END-IF.

*終了伝票番号以上の場合
     IF     DNS-F01  >  PARA-EDDENNO
             MOVE      "END"     TO   END-FLG
     END-IF.

 DNSFILF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理
************************************************************
 300-END-SEC            SECTION.

     MOVE   IN-CNT               TO   DSP-IN.
     MOVE   OUT-CNT              TO   DSP-OUT.
     DISPLAY  DSP-IN-AREA      UPON   CONS.
     DISPLAY  DSP-OUT-AREA     UPON   CONS.

     CLOSE  DNSFILF.

 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
