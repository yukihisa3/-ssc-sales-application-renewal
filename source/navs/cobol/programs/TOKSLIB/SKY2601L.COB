****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　送受信                            *
*    モジュール名　　　　：　本社データ送信結果リスト          *
*    作成日／作成者　　　：　2000/06/08 Y.YOSHIDA              *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY2601L.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
     CONSOLE            IS    CONS
     STATION            IS    STAT
     YA                 IS    2-0PITCH
     YB                 IS    1-5PITCH
     YB-21              IS    3-0PITCH
     YA-22              IS    4-0PITCH.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
****
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  プリントファイル  >>********************************
 FD    PRINTF.
 01    P-REC                 PIC  X(200).
*
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
*
*日付／時刻
 01  SYS-DATE                PIC  9(08).
 01  FILLER                  REDEFINES   SYS-DATE.
     03  FILLER              PIC  9(02).
     03  SYS-YMD.
       05  SYS-YY            PIC  9(02).
       05  SYS-MM            PIC  9(02).
       05  SYS-DD            PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*
****  カウンタ  ****
 01  CNT-AREA.
     03  P-CNT               PIC  9(07)  VALUE  ZERO.
*
****  見出し行（表題）  ****
 01  HYODAI.
     03  FILLER              PIC  X(01)  VALUE  SPACE.
     03  FILLER              PIC  X(08)  VALUE  "SKY2601L".
     03  FILLER              PIC  X(34)  VALUE  SPACE.
     03  FILLER              PIC  N(20)  VALUE
         NC"＃＃＃　本社データ送信処理　＃＃＃"
                             CHARACTER   TYPE   IS   3-0PITCH.
****  見出し行０  ****
 01  MIDASHI-0       CHARACTER    TYPE  IS  2-0PITCH.
     03  FILLER              PIC  X(55)  VALUE  SPACE.
     03  H-YY                PIC  9(04).
     03  FILLER              PIC  N(01)  VALUE  NC"年".
     03  H-MM                PIC  Z9.
     03  FILLER              PIC  N(01)  VALUE  NC"月".
     03  H-DD                PIC  Z9.
     03  FILLER              PIC  N(01)  VALUE  NC"日".
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  H-HH                PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-MI                PIC  9(02).
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-SS                PIC  9(02).
*
****  明細１  ****
 01  MEISAI-1        CHARACTER    TYPE  IS  4-0PITCH.
     03  FILLER              PIC  X(35)  VALUE  SPACE.
     03  M1-PARA1            PIC  N(02).
     03  FILLER              PIC  N(08)  VALUE
         NC"データ送信処理は".
     03  M1-PARA2            PIC  N(02).
     03  FILLER              PIC  N(07)  VALUE
         NC"終了しました。".
*
****  メッセージ情報  ****
 01  MSG-AREA.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE  "SKY2601L".
       05  FILLER            PIC  X(13)  VALUE  " ABEND    ###".

     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD          PIC  X(05).
       05  FILLER            PIC  X(05)  VALUE  " ###".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****  パラメータ  ****
 LINKAGE                SECTION.
 01  LINK-PARA1              PIC  X(01).
 01  LINK-PARA2              PIC  X(01).
************************************************************
*      ＭＡＩＮ              ＭＯＤＵＬＥ                  *
************************************************************
*
 PROCEDURE           DIVISION     USING  LINK-PARA1
                                         LINK-PARA2.
*
************************************************************
*      <0.0>    メインモジュール                           *
************************************************************
 SKY2601L-START         SECTION.
     DISPLAY  "**  SKY2601L   START  **"   UPON  CONS.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC.
     PERFORM            END-SEC.
*
     DISPLAY  "**  SKY2601L    END   **"   UPON  CONS.
     STOP               RUN.
 SKY2601L-END.
     EXIT.
************************************************************
*      <1.0>    初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*----  ファイルＯＰＥＮ  ----*
     OPEN     OUTPUT   PRINTF.
*----  システム日付の格納  ----*
     ACCEPT      SYS-YMD     FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*システム日付・時刻の取得
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   H-YY.
     MOVE      SYS-DATE(5:2)      TO   H-MM.
     MOVE      SYS-DATE(7:2)      TO   H-DD.
*画面表示時刻編集
     MOVE      SYS-TIME(1:2)      TO   H-HH.
     MOVE      SYS-TIME(3:2)      TO   H-MI.
     MOVE      SYS-TIME(5:2)      TO   H-SS.
*
 INIT-EXIT.
     EXIT.
************************************************************
*      <2.0>  メイン処理                                   *
************************************************************
 MAIN-SEC               SECTION.
*
     EVALUATE      LINK-PARA1
        WHEN  "1"
              MOVE      NC"売上"       TO   M1-PARA1
        WHEN  "2"
              MOVE      NC"仕入"       TO   M1-PARA1
        WHEN  OTHER
              MOVE      NC"？？"       TO   M1-PARA1
     END-EVALUATE.
*
     EVALUATE      LINK-PARA2
        WHEN  "1"
              MOVE      NC"正常"       TO   M1-PARA2
        WHEN  "2"
              MOVE      NC"異常"       TO   M1-PARA2
        WHEN  OTHER
              MOVE      NC"？？"       TO   M1-PARA2
     END-EVALUATE.
*
     MOVE     SPACE               TO   P-REC.
     INITIALIZE                        P-REC.
     MOVE     1                   TO   P-CNT.
*
     WRITE    P-REC     FROM      HYODAI      AFTER  2.
     WRITE    P-REC     FROM      MIDASHI-0   AFTER  5.
     WRITE    P-REC     FROM      MEISAI-1    AFTER  23.
*
 MAIN-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
*----  ファイルＣＬＯＳＥ  ----*
     CLOSE    PRINTF.
*
     DISPLAY "* PRINTF(PAGE)=" P-CNT    " *" UPON CONS.
 END-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
